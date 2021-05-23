{-# LANGUAGE QuasiQuotes #-}

module SQLite.BinaryCache (module SQLite.BinaryCache, module FieldType) where

import Prelude hiding (lookup)

import           Control.Arrow                    ((&&&))
import           Control.DeepSeq                  (NFData)
import qualified Control.Exception
import           Control.Foldl                    (FoldM(..))
import qualified Control.Foldl
import qualified Data.Bool
import           Data.ByteString                  (ByteString)
import           Data.Data                        (Data)
import qualified Data.Maybe
import           Data.Profunctor                  (lmap)
import           Data.String                      (IsString)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           Data.Vector                      (Vector)
import           Database.SQLite.Simple
  ( Connection
  , FromRow
  , NamedParam((:=))
  , Query(..)
  , ToRow
  )
import qualified Database.SQLite.Simple           as Sqlite
import           Database.SQLite.Simple.FromField (FromField(..))
import           Database.SQLite.Simple.ToField   (ToField(..))
import           GHC.Generics                     (Generic)
import           NeatInterpolation                (text)

import           SQLite.BinaryCache.FieldType (FieldType(..), ToFieldType(..))
import qualified SQLite.BinaryCache.FieldType as FieldType

data CacheResult a
  = CacheMiss
  | CacheHit !UTCTime !a
  deriving (Eq, Ord, Show, Generic, Data, NFData, Functor, Foldable, Traversable)

cacheResult :: r -> (UTCTime -> a -> r) -> CacheResult a -> r
cacheResult miss hit = \case
  CacheMiss    -> miss
  CacheHit t a -> hit t a

newtype TableName = TableName { getTestTable :: Text }
  deriving newtype (Show, Ord, Eq, IsString, Semigroup, Monoid)

data Cache i = Cache
  { cacheConn  :: !Connection
  , cacheTable :: !TableName
  } deriving Generic

data Cached i = Cached
  { cachedKey        :: !i
  , cachedData       :: !ByteString
  , cachedModifiedAt :: !UTCTime
  , cachedValidity   :: !Validity
  } deriving Generic

instance FromField i => FromRow (Cached i) where
  fromRow = Cached <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*> Sqlite.field

data Validity
  = Valid
  | Invalid
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

validity :: r -> r -> Validity -> r
validity invalid valid = \case
  Invalid -> invalid
  Valid   -> valid

instance ToField Validity where
  toField = \case
    Valid   -> toField True
    Invalid -> toField False

instance FromField Validity where
  fromField = fmap (Data.Bool.bool Invalid Valid) . fromField

type CacheKey i = (ToFieldType i, ToField i, FromField i)

withCache
  :: forall i a b r
   . CacheKey i
  => FilePath
  -> TableName
  -> (a -> ByteString)
  -> (ByteString -> b)
  -> (i -> IO a)
  -> ((i -> IO (CacheResult b)) -> IO r)
  -> IO r
withCache db table enc dec fn action =
    Sqlite.withConnection db
  $ \conn -> action $ cached (Cache @i conn table) enc dec fn

cached
  :: CacheKey i
  => Cache i
  -> (a -> ByteString)
  -> (ByteString -> b)
  -> (i -> IO a)
  -> i
  -> IO (CacheResult b)
cached cache enc dec fn i = lookup cache i >>= maybe mkCacheMiss mkCacheHit
  where
    mkCacheMiss = fn i >>= \r -> CacheMiss <$ write cache i (enc r)
    mkCacheHit  = pure . fmap dec . uncurry CacheHit . (cachedModifiedAt &&& cachedData)

invalidate :: CacheKey i => Cache i -> i -> IO ()
invalidate cache@(Cache conn table) i =
    withCreateTable cache
  $ Sqlite.executeNamed conn query params
  where
    query = Query (validityQueryFragment table <> "\nwhere id = :id")
    params = [ ":id" := i, ":validity" := Invalid ]

invalidateOlderThan :: CacheKey i => Cache i -> UTCTime -> IO ()
invalidateOlderThan cache@(Cache conn table) t =
    withCreateTable cache
  $ Sqlite.executeNamed conn query params
  where
    query = Query (validityQueryFragment table <> "\nwhere modified_at < :datetime")
    params = [ ":datetime" := t, ":validity" := Invalid ]

write :: CacheKey i => Cache i -> i -> ByteString -> IO ()
write cache i b = upsert cache i b Valid

deleteInvalid :: Cache i -> IO ()
deleteInvalid (Cache conn (TableName table)) =
  Sqlite.executeNamed conn
    (Query [text| delete from $table where validity = :validity |])
    [ ":validity" := Invalid ]

delete :: CacheKey i => Cache i -> i -> IO ()
delete (Cache conn (TableName table)) i =
  Sqlite.executeNamed conn
    (Query [text| delete from $table where id = :id |])
    [ ":id" := i ]

upsert :: CacheKey i => Cache i -> i -> ByteString -> Validity -> IO ()
upsert cache@(Cache conn (TableName table)) i b v =
    withCreateTable cache
  $ Sqlite.executeNamed conn query params
  where
    query = Query
      [text|
        insert into $table (id, data, modified_at, validity)
        values (:id, :data, datetime('now'), :validity)
        on conflict (id) do update
        set data = excluded.data,
            modified_at = excluded.modified_at,
            validity = excluded.validity
      |]
    params =
      [ ":id" := i
      , ":data" := b
      , ":validity" := v
      ]

lookup :: CacheKey i => Cache i -> i -> IO (Maybe (Cached i))
lookup cache@(Cache conn table) i =
    withCreateTable cache
  $ fmap Data.Maybe.listToMaybe
  $ Sqlite.queryNamed conn
      (Query $ selectQueryFragment table <> "\nwhere id = :id and validity = :validity")
      [ ":id" := i, ":validity" := Valid ]

selectRow :: CacheKey i => Cache i -> i -> IO (Maybe (Cached i))
selectRow cache@(Cache conn table) i =
    withCreateTable cache
  $ fmap Data.Maybe.listToMaybe
  $ Sqlite.queryNamed conn
      (Query $ selectQueryFragment table <> "\nwhere id = :id")
      [ ":id" := i ]

selectAllRows :: CacheKey i => Cache i -> IO (Vector (Cached i))
selectAllRows cache = foldAllRows cache Control.Foldl.vectorM

foldAllRows :: CacheKey i => Cache i -> FoldM IO (Cached i) b -> IO b
foldAllRows cache@(Cache conn (TableName table)) f =
    withCreateTable cache
  $ fold_ conn (Query [text| select id, data, modified_at, validity from $table |]) f

selectField :: (CacheKey i, FromField a) => Cache i -> Text -> IO (Vector a)
selectField cache@(Cache conn (TableName table)) field =
    withCreateTable cache
  $ fold_ conn (Query [text| select $field from $table |])
  $ lmap Sqlite.fromOnly Control.Foldl.vectorM

withCreateTable :: CacheKey i => Cache i -> IO a -> IO a
withCreateTable cache action = Control.Exception.try action >>= \case
  Right a -> pure a
  Left e  -> case e of
    (Sqlite.SQLError Sqlite.ErrorError _ _) -> createTable cache *> action
    _                                       -> Control.Exception.throw e

createTable :: forall i. CacheKey i => Cache i -> IO ()
createTable (Cache conn table) =
    Sqlite.execute_ conn
  $ mkTableQuery table (toFieldType @i)
      [ ("data", FieldBlob)
      , ("validity", FieldInteger)
      , ("modified_at", FieldText)
      ]

mkTableQuery :: TableName -> FieldType -> [(Text, FieldType)] -> Query
mkTableQuery (TableName table) pkType fieldSpec =
  let fields = toFieldDefs fieldSpec
      pkTypeTxt = FieldType.render pkType
  in Query [text|
       create table if not exists $table (
         id $pkTypeTxt primary key,
         $fields
       )
     |]

validityQueryFragment :: TableName -> Text
validityQueryFragment (TableName table) =
  [text|
    update $table
    set validity = :validity,
        modified_at = datetime('now')
  |]

selectQueryFragment :: TableName -> Text
selectQueryFragment (TableName table) =
  [text|
    select id, data, modified_at, validity
    from $table
  |]

toFieldDefs :: [(Text, FieldType)] -> Text
toFieldDefs =
    Text.intercalate ",\n"
  . fmap
      ( \(name, sqlData) ->
          let sqlName = FieldType.render sqlData
          in [text| $name $sqlName not null |]
      )

-- Versions of Database.SQLite.Simple.fold* that work with Folds from Control.Foldl

fold :: (FromRow a, ToRow ps) => Connection -> Query -> ps -> FoldM IO a b -> IO b
fold conn query ps = sqliteFold $ Sqlite.fold conn query ps

fold_ :: FromRow a => Connection -> Query -> FoldM IO a b -> IO b
fold_ conn query = sqliteFold $ Sqlite.fold_ conn query

foldNamed :: FromRow a => Connection -> Query -> [NamedParam] -> FoldM IO a b -> IO b
foldNamed conn query ps = sqliteFold $ Sqlite.foldNamed conn query ps

sqliteFold :: (forall x. x -> (x -> a -> IO x) -> IO x) -> FoldM IO a b -> IO b
sqliteFold f (FoldM step initial extract) = initial >>= \b -> f b step >>= extract
