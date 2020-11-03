{-# Language QuasiQuotes #-}

module Main where

import qualified Control.Foldl
import qualified Data.Bool
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Foldable          (traverse_)
import qualified Data.Foldable
import qualified Data.Maybe
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import qualified Data.Time              as Time
import           Data.Vector            (Vector)
import           Database.SQLite.Simple (Connection, Query (..))
import qualified Database.SQLite.Simple as Sqlite
import           NeatInterpolation      (text)
import           SQLite.BinaryCache     (CacheAction (..), CacheResult(..), FieldType(..), Codec(..))
import qualified SQLite.BinaryCache     as Cache
import           Test.Tasty             (TestName, TestTree)
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.Golden      as Golden
import           Test.Tasty.HUnit       (Assertion)
import qualified Test.Tasty.HUnit       as HUnit

main :: IO ()
main = do
  Tasty.defaultMain $ Tasty.testGroup "Tests" [ goldenTests, unitTests ]

withDb :: (Connection -> IO a) -> IO a
withDb = Sqlite.withConnection ":memory:"

testTable :: Text
testTable = "table1"

testRecs :: [(Int, ByteString, Bool)]
testRecs =
  [ (1, "foo", False)
  , (2, "bar", False)
  , (3, "baz", True)
  ]

setupTestDb :: Connection -> IO ()
setupTestDb conn = do
  Cache.createTable testTable conn
  traverse_ go testRecs
  where
    go (a, b, c) = do
      Cache.upsert conn testTable a b c
      Sqlite.execute_ conn query
    -- We do this so that we can test invalidateOlderThan and test "upsert - existing" without
    -- using a threadDelay
    query = Query
      [text|
        update $testTable
        set modified_at = datetime('now', '-10 seconds')
        where id = 1
      |]

dbTestCase :: TestName -> (Connection -> IO ()) -> TestTree
dbTestCase name test = HUnit.testCase name $ withDb (\conn -> setupTestDb conn *> test conn)

unitTests :: TestTree
unitTests = Tasty.testGroup "Unit tests"
  [ Tasty.testGroup "withCreateTable"
    [ dbTestCase "Creates tables when they don't exist" $ \conn -> do
        Cache.withCreateTable conn "some_table" $ Sqlite.execute_ conn "select * from some_table"
        Sqlite.execute_ conn "select * from some_table"
    ]

  , Tasty.testGroup "selects"
    [ dbTestCase "selectField" $ \conn -> do
        (rs :: Vector Int) <- Cache.selectField conn testTable "id"
        assertSameElems rs [1, 2, 3]

    , dbTestCase "foldAllRows" $ \conn -> do
        rs <- Cache.foldAllRows conn testTable
             $ Control.Foldl.generalize
             $ Control.Foldl.list
        assertSameElems testRecs (fmap tup4Init rs)

    , dbTestCase "selectRow" $ \conn -> do
        Just rs <- sequenceA <$> traverse (Cache.selectRow conn testTable) [1, 2, 3]
        assertSameElems (fmap tup3Tail testRecs) (fmap tup3Init rs)

    , dbTestCase "lookup" $ \conn -> do
        mrs <- testTableLookup conn
        assertSameElems (fmap (\(_,b,i) -> Data.Bool.bool (Just b) Nothing i) testRecs) mrs
    ]

  , Tasty.testGroup "inserts"
    [ dbTestCase "upsert - new" $ \conn -> do
        Cache.upsert conn testTable 999 "quux" False
        Just (_, bs) <- Cache.lookup conn testTable 999
        HUnit.assertEqual "" "quux" bs

    , dbTestCase "upsert - existing" $ \conn -> do
        Just (oldTime, _) <- Cache.lookup conn testTable 1
        Cache.upsert conn testTable 1 "foo2" True
        Just (newFoo, invalid, newTime) <- Cache.selectRow conn testTable 1
        HUnit.assertEqual "data updated" "foo2" newFoo
        HUnit.assertBool "Is invalid" invalid
        HUnit.assertBool "modified_at is newer" $ newTime > oldTime
    ]

  , Tasty.testGroup "deletes"
    [ dbTestCase "delete" $ \conn -> do
        Cache.delete conn testTable 1
        rs <- testTableRows conn
        assertSameElems (tail testRecs) rs

    , dbTestCase "deleteInvalid" $ \conn -> do
        Cache.deleteInvalid conn testTable
        rs <- testTableRows conn
        assertSameElems (init testRecs) rs
    ]

  -- TODO Do not rely on the exact structure of testRecs
  , Tasty.testGroup "invalidation"
    [ dbTestCase "invalidate" $ \conn -> do
        Cache.invalidate conn testTable 1
        Cache.invalidate conn testTable 3
        mrs <- testTableLookup conn
        assertSameElems [Nothing, Nothing, Just "bar"] mrs

    , dbTestCase "invalidateOlderThan" $ \conn -> do
        now <- Time.getCurrentTime
        let m5 = Time.addUTCTime (-5) now
        Cache.invalidateOlderThan conn testTable m5
        mrs <- testTableLookup conn
        assertSameElems [Nothing, Nothing, Just "bar"] mrs
    ]

  , Tasty.testGroup "traverseCache_"
    [ dbTestCase "NoAction does nothing" $ \conn -> do
        Cache.traverseCache_ conn testTable (const (pure NoAction))
        rs <- testTableRows conn
        assertSameElems testRecs rs

    , dbTestCase "Can Modify elements" $ \conn -> do
        let modify _ = Modify "modded" False
        Cache.traverseCache_ conn testTable (pure . modify)
        rs <- testTableRows conn
        assertSameElems (applyCacheAction modify testRecs) rs

    , dbTestCase "Can Delete elements" $ \conn -> do
        Cache.traverseCache_ conn testTable (const (pure Delete))
        rs <- testTableRows conn
        assertSameElems [] rs

    , dbTestCase "Can combine different actions" $ \conn -> do
        let modify i | i == 1 = NoAction
                     | i == 2 = Modify "new" False
                     | otherwise = Delete
        Cache.traverseCache_ conn testTable (\(i,_,_,_) -> pure $ modify i)
        rs <- testTableRows conn
        assertSameElems (applyCacheAction modify testRecs) rs
    ]

  , Tasty.testGroup "cached"
    [ dbTestCase "CacheHit for existing data" $ \conn -> do
        Just (_,foo) <- Cache.lookup conn testTable 1
        CacheHit _ x <- Cache.cached conn testTable (Codec id id) (const $ pure "nope") 1
        HUnit.assertEqual "" foo x

    , dbTestCase "CacheMiss for new data" $ \conn -> do
        r <- Cache.cached conn testTable (Codec id id) (const $ pure "miss") 999
        HUnit.assertEqual "" (CacheMiss "miss") r

    , dbTestCase "Inserts new data when there is a miss" $ \conn -> do
        Cache.cached conn testTable (Codec id id) (const $ pure "miss") 999
        rs <- testTableRows conn
        assertSameElems ((999, "miss", False) : testRecs) rs

    , dbTestCase "upserts when old data is invalid" $ \conn -> do
        Cache.cached conn testTable (Codec id id) (const $ pure "newbaz") 3
        rs <- testTableRows conn
        let modify i | i == 3 = Modify "newbaz" False
                     | otherwise = NoAction
        assertSameElems (applyCacheAction modify testRecs) rs
    ]

  ]

goldenVsText :: TestName -> FilePath -> Text -> TestTree
goldenVsText n p t = Golden.goldenVsString n p (pure $ LBS.fromStrict $ Text.encodeUtf8 t)

goldenTests :: TestTree
goldenTests = Tasty.testGroup "Golden tests"
  [ goldenVsText "mkTableQuery" "test/mkTableQuery.golden"
    $ Sqlite.fromQuery $ Cache.mkTableQuery "test"
        [ ("textf", FieldText)
        , ("integerf", FieldInteger)
        , ("blobf", FieldBlob)
        , ("realf", FieldReal)
        ]
  ]

assertSameElems :: (Show a, Ord a, Foldable f, Foldable g) => f a -> g a -> Assertion
assertSameElems f g = HUnit.assertEqual "" (setFromFoldable f) (setFromFoldable g)

setFromFoldable :: (Ord a, Foldable f) => f a -> Set a
setFromFoldable = Set.fromList . Data.Foldable.toList

testTableRows :: Connection -> IO (Vector (Int, ByteString, Bool))
testTableRows conn = fmap tup4Init <$> Cache.selectAllRows conn testTable

testTableLookup :: Connection -> IO [Maybe ByteString]
testTableLookup conn =
  (fmap . fmap) snd <$> traverse (\(i,_,_) -> Cache.lookup conn testTable i) testRecs

applyCacheAction :: (Int -> CacheAction) -> [(Int, ByteString, Bool)] -> [(Int, ByteString, Bool)]
applyCacheAction f = Data.Maybe.mapMaybe go
  where
    go x@(i, _, _) = case f i of
      NoAction      -> Just x
      Modify bs' b' -> Just (i, bs', b')
      Delete        -> Nothing

tup4Init :: (a, b, c, d) -> (a, b, c)
tup4Init (a, b, c, _) = (a, b, c)

tup3Init :: (a, b, c) -> (a, b)
tup3Init (a, b, _) = (a, b)

tup3Tail :: (a, b, c) -> (b, c)
tup3Tail (_, b, c) = (b, c)
