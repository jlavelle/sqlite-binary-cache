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
import           Database.SQLite.Simple (Query (..))
import qualified Database.SQLite.Simple as Sqlite
import           NeatInterpolation      (text)
import           SQLite.BinaryCache
    (Cache (..), CacheAction (..), CacheResult (..), Codec (..), FieldType (..))
import qualified SQLite.BinaryCache     as Cache
import           Test.Tasty             (TestName, TestTree)
import qualified Test.Tasty             as Tasty
import qualified Test.Tasty.Golden      as Golden
import           Test.Tasty.HUnit       (Assertion)
import qualified Test.Tasty.HUnit       as HUnit

main :: IO ()
main = do
  Tasty.defaultMain $ Tasty.testGroup "Tests" [ goldenTests, unitTests ]

withCache :: (Cache Int -> IO a) -> IO a
withCache action = Sqlite.withConnection ":memory:" $ \conn -> action (Cache conn testTable)

testTable :: Text
testTable = "table1"

testRecs :: [(Int, ByteString, Bool)]
testRecs =
  [ (1, "foo", False)
  , (2, "bar", False)
  , (3, "baz", True)
  ]

setupTestDb :: Cache Int -> IO ()
setupTestDb cache@(Cache conn _) = do
  Cache.createTable cache
  traverse_ go testRecs
  where
    go (a, b, c) = do
      Cache.upsert cache a b c
      Sqlite.execute_ conn query
    -- We do this so that we can test invalidateOlderThan and test "upsert - existing" without
    -- using a threadDelay
    query = Query
      [text|
        update $testTable
        set modified_at = datetime('now', '-10 seconds')
        where id = 1
      |]

dbTestCase :: TestName -> (Cache Int -> IO ()) -> TestTree
dbTestCase name test = HUnit.testCase name $ withCache (\cache -> setupTestDb cache *> test cache)

unitTests :: TestTree
unitTests = Tasty.testGroup "Unit tests"
  [ Tasty.testGroup "withCreateTable"
    [ dbTestCase "Creates tables when they don't exist" $ \(Cache conn _) -> do
        Cache.withCreateTable (Cache @Int conn "some_table")
          $ Sqlite.execute_ conn "select * from some_table"
        Sqlite.execute_ conn "select * from some_table"
    ]

  , Tasty.testGroup "selects"
    [ dbTestCase "selectField" $ \cache -> do
        (rs :: Vector Int) <- Cache.selectField cache "id"
        assertSameElems rs [1, 2, 3]

    , dbTestCase "foldAllRows" $ \cache -> do
        rs <- Cache.foldAllRows cache
                $ Control.Foldl.generalize
                $ Control.Foldl.list
        assertSameElems testRecs (fmap tup4Init rs)

    , dbTestCase "selectRow" $ \cache -> do
        Just rs <- sequenceA <$> traverse (Cache.selectRow cache) [1, 2, 3]
        assertSameElems (fmap tup3Tail testRecs) (fmap tup3Init rs)

    , dbTestCase "lookup" $ \cache -> do
        mrs <- testTableLookup cache
        assertSameElems (fmap (\(_,b,i) -> Data.Bool.bool (Just b) Nothing i) testRecs) mrs
    ]

  , Tasty.testGroup "inserts"
    [ dbTestCase "upsert - new" $ \cache -> do
        Cache.upsert cache 999 "quux" False
        Just (_, bs) <- Cache.lookup cache 999
        HUnit.assertEqual "" "quux" bs

    , dbTestCase "upsert - existing" $ \cache -> do
        Just (oldTime, _) <- Cache.lookup cache 1
        Cache.upsert cache 1 "foo2" True
        Just (newFoo, invalid, newTime) <- Cache.selectRow cache 1
        HUnit.assertEqual "data updated" "foo2" newFoo
        HUnit.assertBool "Is invalid" invalid
        HUnit.assertBool "modified_at is newer" $ newTime > oldTime
    ]

  , Tasty.testGroup "deletes"
    [ dbTestCase "delete" $ \cache -> do
        Cache.delete cache 1
        rs <- testTableRows cache
        assertSameElems (tail testRecs) rs

    , dbTestCase "deleteInvalid" $ \cache -> do
        Cache.deleteInvalid cache
        rs <- testTableRows cache
        assertSameElems (init testRecs) rs
    ]

  -- TODO Do not rely on the exact structure of testRecs
  , Tasty.testGroup "invalidation"
    [ dbTestCase "invalidate" $ \cache -> do
        Cache.invalidate cache 1
        Cache.invalidate cache 3
        mrs <- testTableLookup cache
        assertSameElems [Nothing, Nothing, Just "bar"] mrs

    , dbTestCase "invalidateOlderThan" $ \cache -> do
        now <- Time.getCurrentTime
        let m5 = Time.addUTCTime (-5) now
        Cache.invalidateOlderThan cache m5
        mrs <- testTableLookup cache
        assertSameElems [Nothing, Nothing, Just "bar"] mrs
    ]

  , Tasty.testGroup "traverseCache_"
    [ dbTestCase "NoAction does nothing" $ \cache -> do
        Cache.traverseCache_ cache (const (pure NoAction))
        rs <- testTableRows cache
        assertSameElems testRecs rs

    , dbTestCase "Can Modify elements" $ \cache -> do
        let modify _ = Modify "modded" False
        Cache.traverseCache_ cache (pure . modify)
        rs <- testTableRows cache
        assertSameElems (applyCacheAction modify testRecs) rs

    , dbTestCase "Can Delete elements" $ \cache -> do
        Cache.traverseCache_ cache (const (pure Delete))
        rs <- testTableRows cache
        assertSameElems [] rs

    , dbTestCase "Can combine different actions" $ \cache -> do
        let modify i | i == 1 = NoAction
                     | i == 2 = Modify "new" False
                     | otherwise = Delete
        Cache.traverseCache_ cache (\(i,_,_,_) -> pure $ modify i)
        rs <- testTableRows cache
        assertSameElems (applyCacheAction modify testRecs) rs
    ]

  , Tasty.testGroup "cached"
    [ dbTestCase "CacheHit for existing data" $ \cache -> do
        Just (_,foo) <- Cache.lookup cache 1
        CacheHit _ x <- Cache.cached cache (Codec id id) (const $ pure "nope") 1
        HUnit.assertEqual "" foo x

    , dbTestCase "CacheMiss for new data" $ \cache -> do
        r <- Cache.cached cache (Codec id id) (const $ pure "miss") 999
        HUnit.assertEqual "" (CacheMiss "miss") r

    , dbTestCase "Inserts new data when there is a miss" $ \cache -> do
        Cache.cached cache (Codec id id) (const $ pure "miss") 999
        rs <- testTableRows cache
        assertSameElems ((999, "miss", False) : testRecs) rs

    , dbTestCase "upserts when old data is invalid" $ \cache -> do
        Cache.cached cache (Codec id id) (const $ pure "newbaz") 3
        rs <- testTableRows cache
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
    $ Sqlite.fromQuery $ Cache.mkTableQuery "test" FieldInteger
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

testTableRows :: Cache Int -> IO (Vector (Int, ByteString, Bool))
testTableRows cache = fmap tup4Init <$> Cache.selectAllRows cache

testTableLookup :: Cache Int -> IO [Maybe ByteString]
testTableLookup cache =
  (fmap . fmap) snd <$> traverse (\(i,_,_) -> Cache.lookup cache i) testRecs

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
