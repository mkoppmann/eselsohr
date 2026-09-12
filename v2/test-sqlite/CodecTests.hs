{- |
Module      : CodecTests
Description : Stored value and SQL row conversion tests

'codecTests' checks article and grant conversions, UUID byte order, exact timestamp encoding, and revision bounds.
Database cases exercise the SQL row instances and check that malformed values and conversion exceptions are handled without exposing stored data.
-}
module CodecTests (codecTests) where

import Control.Exception (AsyncException (..), fromException, throwIO, toException, try)
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.List (subsequences)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, picosecondsToDiffTime)
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.Ok (ManyErrors (..))
import Eselsohr.Article
import Eselsohr.Capability (GrantSnapshot (..))
import Eselsohr.Store.SQLite.Codec
import SqliteFixtures
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

codecTests :: TestTree
codecTests = testGroup "Storage codecs" [articleTests, timeTests, grantTests, sqlTests]

------------------------------------------------------------------------
-- Articles
------------------------------------------------------------------------

articleTests :: TestTree
articleTests =
    testGroup
        "Articles"
        [ testCase "UUID network order and article values stored in SQLite" $ do
            row <- requireRight (toArticleRow articleFixture)

            rowArticleId row @?= ByteString.init collectionBytes <> ByteString.singleton 2
            rowTitle row @?= "A title"
            rowUrl row @?= "https://example.com/"
            rowState row @?= "unread"
            rowCreatedAt row @?= 1234567
            rowRevision row @?= 0

            toCollectionBytes collection @?= Right collectionBytes
            fromCollectionBytes collectionBytes @?= Right collection
            assertSame (Right articleFixture) (fromArticleRow row)
        , testCase "read state is explicit" $ do
            let article = articleFixture{storedArticle = (storedArticle articleFixture){articleState = Read}}
            row <- requireRight (toArticleRow article)
            rowState row @?= "read"
            assertSame (Right article) (fromArticleRow row)
        , testCase "revision limits are checked before narrowing" $ do
            forM_ [-1, toInteger (maxBound :: Int64) + 1] $ \revision ->
                assertLeft UnrepresentableValue (toArticleRow articleFixture{storedRevision = revision})

            let article = articleFixture{storedRevision = toInteger (maxBound :: Int64)}
            row <- requireRight (toArticleRow article)
            rowRevision row @?= maxBound
            assertSame (Right article) (fromArticleRow row)

            assertLeft InvalidValue (fromArticleRow row{rowRevision = -1})
        , testCase "invalid IDs and noncanonical or invalid text are rejected" $ do
            row <- requireRight (toArticleRow articleFixture)
            forM_ malformedUuids $ \identifier -> do
                assertLeft InvalidValue (fromArticleRow row{rowArticleId = identifier})
                assertLeft InvalidValue (fromCollectionBytes identifier)
            forM_ ["", " A title", "A title ", Text.replicate 501 "a"] $ \title ->
                assertLeft InvalidValue (fromArticleRow row{rowTitle = title})
            forM_ ["", "javascript:alert(1)", "HTTPS://EXAMPLE.COM/"] $ \url ->
                assertLeft InvalidValue (fromArticleRow row{rowUrl = url})
            assertLeft InvalidValue (fromArticleRow row{rowState = "Read"})
        ]

------------------------------------------------------------------------
-- Exact Unix microseconds
------------------------------------------------------------------------

timeTests :: TestTree
timeTests =
    testGroup
        "Exact Unix microseconds"
        [ testCase "epoch, negative time, and day boundaries" $ do
            fromTimestamp 0 @?= UTCTime (fromGregorian 1970 1 1) 0
            fromTimestamp 1 @?= UTCTime (fromGregorian 1970 1 1) (picosecondsToDiffTime 1000000)
            fromTimestamp (-1) @?= UTCTime (fromGregorian 1969 12 31) (picosecondsToDiffTime 86399999999000000)
            fromTimestamp 86400000000 @?= UTCTime (fromGregorian 1970 1 2) 0
            fromTimestamp (-86400000000) @?= UTCTime (fromGregorian 1969 12 31) 0
            forM_ [minBound, maxBound] $ \micros -> toTimestamp (fromTimestamp micros) @?= Right micros
        , testCase "submicroseconds, leap seconds, and overflow are rejected" $ do
            forM_ [-1, 1, 999999, 86400 * 10 ^ (12 :: Int)] $ \picos ->
                toTimestamp (UTCTime (fromGregorian 1970 1 1) (picosecondsToDiffTime picos)) @?= Left UnrepresentableValue
            toTimestamp (addUTCTime 0.000001 (fromTimestamp maxBound)) @?= Left UnrepresentableValue
            toTimestamp (addUTCTime (-0.000001) (fromTimestamp minBound)) @?= Left UnrepresentableValue
            let article = (storedArticle articleFixture){articleCreatedAt = UTCTime (fromGregorian 1970 1 1) (picosecondsToDiffTime 1)}
            assertLeft UnrepresentableValue (toArticleRow articleFixture{storedArticle = article})
        , localOption (QuickCheckTests 500) . testProperty "every sampled Int64 survives an exact round trip" $ \micros ->
            toTimestamp (fromTimestamp micros) == Right micros
        , localOption (QuickCheckTests 500) . testProperty "article rows round trip timestamps and nonnegative revisions" $ \micros revision readState ->
            let article =
                    articleFixture
                        { storedArticle = (storedArticle articleFixture){articleCreatedAt = fromTimestamp micros, articleState = if readState then Read else Unread}
                        , storedRevision = abs (toInteger (revision :: Int64)) `min` toInteger (maxBound :: Int64)
                        }
             in (toArticleRow article >>= fromArticleRow) == Right article
        ]

------------------------------------------------------------------------
-- Grant assembly
------------------------------------------------------------------------

grantTests :: TestTree
grantTests =
    testGroup
        "Grant assembly"
        [ testCase "root permissions use the specified SQLite strings and enum order" $ do
            (row, permissions) <- requireRight (toGrantRows collection rootFixture)

            rowGrantId row @?= ByteString.init collectionBytes <> ByteString.singleton 3
            rowParentId row @?= Nothing
            rowScopeArticleId row @?= Nothing
            rowExpiresAt row @?= Nothing
            rowDepth row @?= 0
            map rowPermission permissions @?= ["read", "add", "title", "state", "delete", "delegate", "inspect", "revoke"]

            assertSame (Right rootFixture) (fromGrantRows collection row permissions)
            assertSame (Right rootFixture) (fromGrantRows collection row (reverse permissions))
            assertLeft InvalidValue (fromGrantRows collection row (drop 1 permissions))
        , testCase "expired grants and optional petnames round trip unchanged" $ do
            forM_ [Nothing, Just "", Just "  phone\NUL\n"] $ \petname -> do
                let grant = childFixture{storedPetname = petname}
                (row, permissions) <- requireRight (toGrantRows collection grant)
                rowExpiresAt row @?= Just (-1)
                rowPetname row @?= petname
                assertSame (Right grant) (fromGrantRows collection row permissions)
        , testCase "all 256 permission subsets including empty round trip" $
            forM_ (subsequences [minBound .. maxBound]) $ \permissions -> do
                let grant = childFixture{storedGrant = (storedGrant childFixture){grantPermissions = Set.fromList permissions}}
                (row, rows) <- requireRight (toGrantRows collection grant)
                assertSame (Right grant) (fromGrantRows collection row (reverse rows))
        , testCase "permission rows reject duplicates, unknown values, foreign keys, and unbounded input" $ do
            (row, [permission]) <- requireRight (toGrantRows collection childFixture)
            forM_ [[permission, permission], [permission{rowPermission = "owner"}], [permission{rowPermissionGrantId = collectionBytes}], repeat permission] $ \rows ->
                await (pure $! fromGrantRows collection row rows) >>= assertLeft InvalidValue
        , testCase "grant row IDs, hash lengths, parent IDs, deadlines, and depths are checked" $ do
            (row, permissions) <- requireRight (toGrantRows collection childFixture)
            forM_ malformedUuids $ \identifier -> do
                assertLeft InvalidValue (fromGrantRows collection row{rowGrantId = identifier} permissions)
                assertLeft InvalidValue (fromGrantRows collection row{rowParentId = Just identifier} permissions)
                assertLeft InvalidValue (fromGrantRows collection row{rowScopeArticleId = Just identifier} permissions)
            forM_ [0, 31, 33] $ \size ->
                assertLeft InvalidValue (fromGrantRows collection row{rowTokenHash = ByteString.replicate size 0} permissions)
            forM_ [minBound, -1, 0, 5, maxBound] $ \depth ->
                assertLeft InvalidValue (fromGrantRows collection row{rowDepth = depth} permissions)
            assertLeft InvalidValue (fromGrantRows collection row{rowParentId = Just (rowGrantId row)} permissions)
            assertLeft InvalidValue (fromGrantRows collection row{rowExpiresAt = Nothing} permissions)
        , testCase "malformed roots are rejected" $ do
            (row, permissions) <- requireRight (toGrantRows collection rootFixture)
            assertLeft InvalidValue (fromGrantRows collection row{rowDepth = 1} permissions)
            assertLeft InvalidValue (fromGrantRows collection row{rowExpiresAt = Just 0} permissions)
            assertLeft InvalidValue (fromGrantRows collection row{rowScopeArticleId = Just collectionBytes} permissions)
        , testCase "encoding rejects a wrong collection, invalid grant fields or hash length, and times SQLite cannot store" $ do
            assertLeft CollectionMismatch (toGrantRows otherCollection rootFixture)
            assertLeft CollectionMismatch (toGrantRows otherCollection childFixture)
            assertLeft InvalidValue (toGrantRows collection childFixture{storedTokenHash = ByteString.empty})
            forM_ [Nothing, Just (fromTimestamp 0)] $ \expiry ->
                assertLeft InvalidValue (toGrantRows collection rootFixture{storedGrant = (storedGrant rootFixture){grantExpiresAt = expiry, grantDepth = 1}})
            assertLeft InvalidValue (toGrantRows collection childFixture{storedGrant = (storedGrant childFixture){grantParentId = Just childIdentifier}})
            let precise = UTCTime (fromGregorian 1970 1 1) (picosecondsToDiffTime 1)
            assertLeft UnrepresentableValue (toGrantRows collection childFixture{storedGrant = (storedGrant childFixture){grantExpiresAt = Just precise}})
        ]

------------------------------------------------------------------------
-- SQL instances
------------------------------------------------------------------------

sqlTests :: TestTree
sqlTests =
    testGroup
        "SQL instances"
        [ testCase "article and grant DTOs round trip through SQLite" . withDatabase $ \connection -> do
            insertArticle connection articleFixture
            insertGrant connection collection rootFixture
            insertGrant connection collection childFixture

            rows <- requireRight =<< catchConversionErrors (SQL.query_ connection "SELECT id, title, url, state, created_at, revision FROM article" :: IO [ArticleRow])
            assertSame (Right [articleFixture]) (traverse fromArticleRow rows)

            forM_ [rootFixture, childFixture] $ \grant -> do
                (expected, _) <- requireRight (toGrantRows collection grant)
                [row] <- requireRight =<< catchConversionErrors (SQL.query connection "SELECT id, token_hash, parent_id, scope_article_id, expires_at, depth, petname FROM capability WHERE id = ?" (Only (rowGrantId expected)) :: IO [GrantRow])
                permissions <- requireRight =<< catchConversionErrors (SQL.query connection "SELECT capability_id, permission FROM capability_permission WHERE capability_id = ? ORDER BY permission DESC" (Only (rowGrantId row)) :: IO [PermissionRow])
                assertSame (Right grant) (fromGrantRows collection row permissions)
        , testCase "same article ID in independent files preserves each value" . withDatabase $ \first -> withDatabase $ \second -> do
            title <- requireRight (mkTitle "second")
            let other = articleFixture{storedArticle = (storedArticle articleFixture){articleTitle = title}}
            insertArticle first articleFixture
            insertArticle second other

            forM_ [(first, articleFixture), (second, other)] $ \(connection, expected) -> do
                rows <- SQL.query_ connection "SELECT id, title, url, state, created_at, revision FROM article"
                assertSame (Right [expected]) (traverse fromArticleRow rows)
        , testCase "wrong article column types, nulls, and column counts return InvalidValue" . withDatabase $ \connection -> do
            row <- requireRight (toArticleRow articleFixture)
            let fields = SQL.toRow row
            forM_ (zip [0 ..] fields) $ \(index, field) -> do
                let wrong = case field of
                        SQL.SQLText _ -> SQL.SQLInteger 1
                        _ -> SQL.SQLText "private stored value"
                forM_ [wrong, SQL.SQLNull] $ \replacement ->
                    queryArticle connection (take index fields <> [replacement] <> drop (index + 1) fields) >>= assertLeft InvalidValue
            catchConversionErrors (SQL.query_ connection "SELECT 'private stored value'" :: IO [ArticleRow]) >>= assertLeft InvalidValue
            catchConversionErrors (SQL.query connection "SELECT ?, ?, ?, ?, ?, ?, ?" (fields <> [SQL.SQLText "extra"]) :: IO [ArticleRow]) >>= assertLeft InvalidValue
        , testCase "invalid values remain a distinct checked decoding step" . withDatabase $ \connection -> do
            row <- requireRight (toArticleRow articleFixture)
            [invalid] <- requireRight =<< queryArticle connection (SQL.toRow row{rowState = "secret invalid state"})
            assertLeft InvalidValue (fromArticleRow invalid)
        , testCase "invalid UTF-8 is rejected at the query boundary" . withDatabase $ \connection ->
            catchConversionErrors (SQL.query_ connection "SELECT CAST(x'ff' AS TEXT)" :: IO [Only Text.Text]) >>= assertLeft InvalidValue
        , testCase "aggregated conversion errors are sanitized without hiding unexpected errors" $ do
            let conversion = toException (SQL.ConversionFailed "TEXT" "Text" "private stored value")
                nested = toException (ManyErrors [conversion])

            catchConversionErrors (throwIO (ManyErrors [conversion, nested]) :: IO ()) >>= (@?= Left InvalidValue)

            result <- try @ManyErrors . catchConversionErrors $ (throwIO (ManyErrors [conversion, toException CallbackFailure]) :: IO ())
            case result of
                Left (ManyErrors [_, unexpected]) -> fromException unexpected @?= Just CallbackFailure
                _ -> assertFailure "unexpected aggregate was swallowed or changed"
        , testCase "grant and permission row conversion failures are sanitized" . withDatabase $ \connection -> do
            (row, [permission]) <- requireRight (toGrantRows collection childFixture)
            let fields = SQL.toRow row
            forM_ [0 .. 6] $ \index ->
                catchConversionErrors (SQL.query connection "SELECT ?, ?, ?, ?, ?, ?, ?" (take index fields <> [SQL.SQLFloat 1.5] <> drop (index + 1) fields) :: IO [GrantRow]) >>= assertLeft InvalidValue
            catchConversionErrors (SQL.query connection "SELECT ?, ?" (rowPermissionGrantId permission, SQL.SQLNull) :: IO [PermissionRow]) >>= assertLeft InvalidValue
            catchConversionErrors (SQL.query_ connection "SELECT NULL, 'read'" :: IO [PermissionRow]) >>= assertLeft InvalidValue
        , testCase "unexpected, SQL, and cancellation exceptions propagate" . withDatabase $ \connection -> do
            try @CallbackFailure (catchConversionErrors (throwIO CallbackFailure :: IO ())) >>= (@?= Left CallbackFailure)
            try @AsyncException (catchConversionErrors (throwIO ThreadKilled :: IO ())) >>= (@?= Left ThreadKilled)
            result <- try @SQL.SQLError . catchConversionErrors $ (SQL.query_ connection "SELECT * FROM missing" :: IO [ArticleRow])
            case result of
                Left _ -> pure ()
                Right _ -> assertFailure "SQL exception was swallowed"
        ]

queryArticle :: Connection -> [SQL.SQLData] -> IO (Either CodecError [ArticleRow])
queryArticle connection fields = catchConversionErrors (SQL.query connection "SELECT ?, ?, ?, ?, ?, ?" fields)

malformedUuids :: [ByteString.ByteString]
malformedUuids =
    [ ByteString.empty
    , ByteString.take 15 collectionBytes
    , collectionBytes <> ByteString.singleton 0
    , ByteString.replicate 16 0
    , ByteString.take 6 collectionBytes <> ByteString.singleton 0x12 <> ByteString.drop 7 collectionBytes
    , ByteString.take 8 collectionBytes <> ByteString.singleton 0x04 <> ByteString.drop 9 collectionBytes
    ]
