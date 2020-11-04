{-# Language QuasiQuotes #-}

module SQLite.BinaryCache (module SQLite.BinaryCache, module FieldType) where

import           Prelude                          hiding (lookup)

import           Control.DeepSeq                  (NFData)
import qualified Control.Exception
import           Control.Foldl                    (FoldM (..))
import qualified Control.Foldl
import           Data.Bifunctor                   (Bifunctor (..))
import           Data.ByteString                  (ByteString)
import           Data.Data                        (Data)
import qualified Data.Maybe
import           Data.Profunctor                  (Profunctor (..))
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           Data.Vector                      (Vector)
import           Database.SQLite.Simple
    (Connection, FromRow, NamedParam ((:=)), Query (..), ToRow)
import qualified Database.SQLite.Simple           as Sqlite
import           Database.SQLite.Simple.FromField (FromField)
import           GHC.Generics                     (Generic)
import           NeatInterpolation                (text)
import           SQLite.BinaryCache.FieldType     (FieldType (..), ToFieldType (..))
import qualified SQLite.BinaryCache.FieldType     as FieldType

data Codec a b c = Codec
  { codecEncode :: b -> a
  , codecDecode :: a -> c
  }
  deriving (Generic, NFData, Functor)

instance Profunctor (Codec a) where
  dimap f g (Codec enc dec) = Codec (enc . f) (g . dec)

invmapCodec :: (a -> x) -> (x -> a) -> Codec a b c -> Codec x b c
invmapCodec to from (Codec enc dec) = Codec (to . enc) (dec . from)

data CacheResult a b
  = CacheMiss a
  | CacheHit UTCTime b
  deriving (Eq, Ord, Show, Generic, Data, NFData, Functor, Foldable, Traversable)

cacheResult :: (a -> c) -> (UTCTime -> b -> c) -> CacheResult a b -> c
cacheResult m h = \case
  CacheMiss a  -> m a
  CacheHit t b -> h t b

instance Bifunctor CacheResult where
  bimap f g = cacheResult (CacheMiss . f) (\t -> CacheHit t . g)

data Cache i = Cache
  { cacheConn  :: Connection
  , cacheTable :: Text
  }
  deriving Generic

withCache
  :: forall i a b r
   . ToFieldType i
  => FilePath
  -> Text
  -> Codec ByteString a b
  -> (i -> IO a)
  -> ((i -> IO (CacheResult a b)) -> IO r)
  -> IO r
withCache db table codec fn action =
  Sqlite.withConnection db $ \conn -> action $ cached (Cache @i conn table) codec fn

cached
  :: ToFieldType i
  => Cache i
  -> Codec ByteString a b
  -> (i -> IO a)
  -> i
  -> IO (CacheResult a b)
cached cache (Codec enc dec) fn i =
  lookup cache i >>= maybe miss (pure . fmap dec . uncurry CacheHit)
  where
    miss = fn i >>= \r -> CacheMiss r <$ write cache i (enc r)

data CacheAction
  = NoAction
  | Modify ByteString Bool
  | Delete
  deriving (Eq, Show, Generic, Data, NFData)

traverseCache_
  :: ToFieldType i
  => Cache i
  -> ((i, ByteString, Bool, UTCTime) -> IO CacheAction)
  -> IO ()
traverseCache_ cache@(Cache conn _) f = Sqlite.withTransaction conn
  $ foldAllRows cache (FoldM (const step) (pure ()) pure)
  where
    step r@(i, _, _, _) = f r >>= \case
      NoAction          -> pure ()
      Modify bs invalid -> upsert cache i bs invalid
      Delete            -> delete cache i

invalidate :: ToFieldType i => Cache i -> i -> IO ()
invalidate cache@(Cache conn table) i = withCreateTable cache
  $ Sqlite.executeNamed conn query params
  where
    query = Query (invalidateQueryFragment table <> "\nwhere id = :id")
    params = [ ":id" := i, ":invalid" := True ]

invalidateOlderThan :: ToFieldType i => Cache i -> UTCTime -> IO ()
invalidateOlderThan cache@(Cache conn table) t = withCreateTable cache
  $ Sqlite.executeNamed conn query params
  where
    query = Query (invalidateQueryFragment table <> "\nwhere modified_at < :datetime")
    params = [ ":datetime" := t, ":invalid" := True ]

write :: ToFieldType i => Cache i -> i -> ByteString -> IO ()
write cache i b = upsert cache i b False

deleteInvalid :: Cache i -> IO ()
deleteInvalid (Cache conn table) = Sqlite.executeNamed conn
  (Query [text| delete from $table where invalid = :invalid |])
  [ ":invalid" := True ]

delete :: ToFieldType i => Cache i -> i -> IO ()
delete (Cache conn table) i =
  Sqlite.executeNamed conn (Query [text| delete from $table where id = :id |]) [ ":id" := i ]

upsert :: ToFieldType i => Cache i -> i -> ByteString -> Bool -> IO ()
upsert cache@(Cache conn table) i b invalid = withCreateTable cache
  $ Sqlite.executeNamed conn query params
  where
    query = Query
      [text|
        insert into $table (id, data, invalid, modified_at)
        values (:id, :data, :invalid, datetime('now'))
        on conflict (id) do update
        set data = excluded.data,
            modified_at = excluded.modified_at,
            invalid = excluded.invalid
      |]
    params =
      [ ":id" := i
      , ":data" := b
      , ":invalid" := invalid
      ]

lookup :: ToFieldType i => Cache i -> i -> IO (Maybe (UTCTime, ByteString))
lookup cache@(Cache conn table) i = withCreateTable cache
  $ fmap Data.Maybe.listToMaybe
  $ Sqlite.queryNamed conn
      (Query [text| select modified_at, data from $table where id = :id and invalid = :invalid |])
      [ ":id" := i, ":invalid" := False ]

selectRow :: ToFieldType i => Cache i -> i -> IO (Maybe (ByteString, Bool, UTCTime))
selectRow cache@(Cache conn table) i = withCreateTable cache
  $ fmap Data.Maybe.listToMaybe
  $ Sqlite.queryNamed conn
      (Query [text| select data, invalid, modified_at from $table where id = :id |])
      [ ":id" := i ]

selectAllRows :: ToFieldType i => Cache i -> IO (Vector (i, ByteString, Bool, UTCTime))
selectAllRows cache = foldAllRows cache $ Control.Foldl.vectorM

foldAllRows :: ToFieldType i => Cache i -> FoldM IO (i, ByteString, Bool, UTCTime) b -> IO b
foldAllRows cache@(Cache conn table) f = withCreateTable cache
  $ fold_ conn (Query [text| select id, data, invalid, modified_at from $table |]) f

selectField :: (ToFieldType i, FromField a) => Cache i -> Text -> IO (Vector a)
selectField cache@(Cache conn table) field = withCreateTable cache
  $ fold_ conn (Query [text| select $field from $table |])
  $ lmap Sqlite.fromOnly Control.Foldl.vectorM

withCreateTable :: ToFieldType i => Cache i -> IO a -> IO a
withCreateTable cache action = Control.Exception.try action >>= \case
  Right a                                      -> pure a
  Left (Sqlite.SQLError Sqlite.ErrorError _ _) -> createTable cache *> action
  Left e                                       -> Control.Exception.throw e

createTable :: forall i. ToFieldType i => Cache i -> IO ()
createTable (Cache conn table) = Sqlite.execute_ conn $ mkTableQuery table (toFieldType @i)
  [ ("data", FieldBlob)
  , ("invalid", FieldInteger)
  , ("modified_at", FieldText)
  ]

mkTableQuery :: Text -> FieldType -> [(Text, FieldType)] -> Query
mkTableQuery table pkType fieldSpec =
  let fields = toFieldDefs fieldSpec
      pkTypeTxt = FieldType.render pkType
  in Query [text|
       create table if not exists $table (
         id $pkTypeTxt primary key,
         $fields
       )
     |]

invalidateQueryFragment :: Text -> Text
invalidateQueryFragment table =
  [text|
    update $table
    set invalid = :invalid,
        modified_at = datetime('now')
  |]

toFieldDefs :: [(Text, FieldType)] -> Text
toFieldDefs =
  Text.intercalate ",\n"
  . fmap
    (\(name, sqlData) ->
      let sqlName = FieldType.render sqlData
      in [text| $name $sqlName not null |])

-- Versions of Database.SQLite.Simple.fold* that work with Folds from Control.Foldl

fold :: (FromRow a, ToRow ps) => Connection -> Query -> ps -> FoldM IO a b -> IO b
fold conn query ps = sqliteFold $ Sqlite.fold conn query ps

fold_ :: FromRow a => Connection -> Query -> FoldM IO a b -> IO b
fold_ conn query = sqliteFold $ Sqlite.fold_ conn query

foldNamed :: FromRow a => Connection -> Query -> [NamedParam] -> FoldM IO a b -> IO b
foldNamed conn query ps = sqliteFold $ Sqlite.foldNamed conn query ps

sqliteFold :: (forall x. x -> (x -> a -> IO x) -> IO x) -> FoldM IO a b -> IO b
sqliteFold f (FoldM step initial extract) = initial >>= \b -> f b step >>= extract
