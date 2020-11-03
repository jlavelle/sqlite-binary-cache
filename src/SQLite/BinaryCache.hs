{-# Language QuasiQuotes #-}

module SQLite.BinaryCache where

import           Prelude                          hiding (lookup)

import           Control.DeepSeq                  (NFData)
import qualified Control.Exception
import           Control.Foldl                    (FoldM (..))
import qualified Control.Foldl
import           Data.ByteString                  (ByteString)
import           Data.Data                        (Data)
import qualified Data.Maybe
import           Data.Profunctor                  (lmap)
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

data CacheAction
  = NoAction
  | Modify ByteString Bool
  | Delete
  deriving (Eq, Show, Generic, Data, NFData)

traverseCache_
  :: Connection
  -> Text
  -> ((Int, ByteString, Bool, UTCTime) -> IO CacheAction)
  -> IO ()
traverseCache_ conn table f = Sqlite.withTransaction conn
  $ foldAllRows conn table (FoldM (const step) (pure ()) pure)
  where
    step r@(i, _, _, _) = f r >>= \case
      NoAction -> pure ()
      Modify bs invalid -> upsert conn table i bs invalid
      Delete -> delete conn table i

invalidate :: Connection -> Text -> Int -> IO ()
invalidate conn table i = withCreateTable conn table
  $ Sqlite.executeNamed conn query params
  where
    query = Query (invalidateQueryFragment table <> "\nwhere id = :id")

    params = [ ":id" := i, ":invalid" := True ]

invalidateOlderThan :: Connection -> Text -> UTCTime -> IO ()
invalidateOlderThan conn table t = withCreateTable conn table
  $ Sqlite.executeNamed conn query params
  where
    query = Query (invalidateQueryFragment table <> "\nwhere modified_at < :datetime")
    params = [ ":datetime" := t, ":invalid" := True ]

write :: Connection -> Text -> Int -> ByteString -> IO ()
write conn table i b = upsert conn table i b False

deleteInvalid :: Connection -> Text -> IO ()
deleteInvalid conn table = Sqlite.executeNamed conn
  (Query [text| delete from $table where invalid = :invalid |])
  [ ":invalid" := True ]

delete :: Connection -> Text -> Int -> IO ()
delete conn table i =
  Sqlite.executeNamed conn (Query [text| delete from $table where id = :id |]) [ ":id" := i ]

upsert :: Connection -> Text -> Int -> ByteString -> Bool -> IO ()
upsert conn table i b invalid = withCreateTable conn table
  $ Sqlite.executeNamed conn query params
  where
    query = Query
      [text|
        insert into $table (id, data, invalid, modified_at)
        values (:id, :data, :invalid, datetime('now'))
        on conflict (id) do update
        set data = excluded.data,
            modified_at = excluded.modified_at
            invalid = excluded.invalid
      |]
    params =
      [ ":id" := i
      , ":data" := b
      , "invalid" := invalid
      ]

lookup :: Connection -> Text -> Int -> IO (Maybe ByteString)
lookup conn table i = withCreateTable conn table
  $ fmap (fmap Sqlite.fromOnly . Data.Maybe.listToMaybe)
  $ Sqlite.queryNamed conn
      (Query [text| select data from $table where id = :id and invalid = :invalid |])
      [ ":id" := i, ":invalid" := False ]

selectRow :: Connection -> Text -> Int -> IO (Maybe (ByteString, Bool, UTCTime))
selectRow conn table i = withCreateTable conn table
  $ fmap Data.Maybe.listToMaybe
  $ Sqlite.queryNamed conn
      (Query [text| select data, invalid, modified_at from $table where id = :id |])
      [ ":id" := i ]

selectAllRows :: Connection -> Text -> IO (Vector (Int, ByteString, Bool, UTCTime))
selectAllRows conn table = foldAllRows conn table $ Control.Foldl.vectorM

foldAllRows :: Connection -> Text -> FoldM IO (Int, ByteString, Bool, UTCTime) b -> IO b
foldAllRows conn table f = withCreateTable conn table
  $ fold_ conn (Query [text| select id, data, invalid, modified_at from $table |]) f

selectField :: FromField a => Connection -> Text -> Text -> IO (Vector a)
selectField conn table field = withCreateTable conn table
  $ fold_ conn (Query [text| select $field from $table |])
  $ lmap Sqlite.fromOnly Control.Foldl.vectorM

withCreateTable :: Connection -> Text -> IO a -> IO a
withCreateTable conn table action = Control.Exception.try action >>= \case
  Right a -> pure a
  Left (Sqlite.SQLError Sqlite.ErrorError _ _) -> createTable table conn *> action
  Left e -> Control.Exception.throw e

data FieldType
  = FieldInteger
  | FieldBlob
  | FieldReal
  | FieldText
  deriving (Eq, Ord, Enum, Show, Generic, Data, NFData)

renderFieldType :: FieldType -> Text
renderFieldType = \case
  FieldInteger -> "integer"
  FieldReal -> "real"
  FieldText -> "text"
  FieldBlob -> "blob"

createTable :: Text -> Connection -> IO ()
createTable table conn = Sqlite.execute_ conn $ mkTableQuery table
  [ ("data", FieldBlob)
  , ("invalid", FieldInteger)
  , ("modified_at", FieldText)
  ]

mkTableQuery :: Text -> [(Text, FieldType)] -> Query
mkTableQuery table fieldSpec =
  let fields = toFieldDefs fieldSpec
  in Query [text|
       create table if not exists $table(
         id integer primary key,
         $fields
       )
     |]

invalidateQueryFragment :: Text -> Text
invalidateQueryFragment table =
  [text|
    update $table
    set invalid = :invalid
        modified_at = datetime('now')
  |]

toFieldDefs :: [(Text, FieldType)] -> Text
toFieldDefs =
  Text.intercalate ",\n"
  . fmap
    (\(name, sqlData) ->
      let sqlName = renderFieldType sqlData
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
