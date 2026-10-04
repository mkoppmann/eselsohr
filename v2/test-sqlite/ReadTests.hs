{- |
Module      : ReadTests
Description : Protected article read tests

'readTests' checks that stored grants authorize exactly the requested collection or article, and that returned articles retain their revisions.
Cases also cover malformed storage, grant changes between transactions, and the transaction clock.

Concurrency and failure tests check lock release, rollback, cancellation, and preservation of the caller's exception masking state.
-}
module ReadTests (readTests) where

import BearerTests (goldenDigest, goldenToken, otherDigest, otherToken, zeroDigest, zeroToken)
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), MaskingState (..), bracket, bracket_, fromException, getMaskingState, mask_, throwIO, try)
import Control.Monad (forM_, void, (>=>))
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Article (Article (..))
import Eselsohr.Capability
import Eselsohr.Id
import Eselsohr.Store.SQLite.Articles
import Eselsohr.Store.SQLite.Bearer (Bearer, parseBearer)
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Open (withExistingFile)
import Eselsohr.Store.SQLite.Schema (initializeSchema)
import Eselsohr.WebUrl (WebUrl, parseWebUrl)
import SqliteFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

readTests :: TestTree
readTests = testGroup "Protected read" [authorityTests, storageTests, transactionTests]

------------------------------------------------------------------------
-- Authority and sealed scope
------------------------------------------------------------------------

authorityTests :: TestTree
authorityTests =
    testGroup
        "Authority and sealed scope"
        [ testCase "named owner operation returns articles with unchanged revisions" . withReadDatabase $ \_ connection bearer -> do
            result <- requireRight =<< readArticles connection bearer (pure instant) (WholeCollection collection)
            sameArticles [articleFixture, secondArticle] result
            readArticles connection bearer (pure instant) (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture])
            assertAutocommit connection
        , testCase "authorized missing articles and empty collections return empty lists" . withReadDatabase $ \_ connection bearer -> do
            missing <- requireRight (parseArticleId "123e4567-e89b-42d3-a456-426614174099")
            readArticles connection bearer (pure instant) (OneArticle collection missing) >>= assertSame (Right [])

            SQL.execute_ connection "DELETE FROM article"
            readArticles connection bearer (pure instant) (WholeCollection collection) >>= assertSame (Right [])
        , testCase "a child reads exactly its article and cannot widen or change collection" . withReadDatabase $ \_ connection _ -> do
            insertGrant connection collection readChild
            bearer <- requireRight (parseBearer zeroToken)
            readAt connection bearer (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture])
            forM_ [WholeCollection collection, OneArticle collection (articleId (storedArticle secondArticle)), OneArticle otherCollection articleIdentifier] $ \scope -> do
                readAt connection bearer scope >>= assertLeft StoreDenied
                assertAutocommit connection
        , testCase "each sealed scope stays intact after checking another request" . withReadDatabase $ \_ connection bearer -> do
            result <-
                requireRight
                    =<< runReadTransaction
                        connection
                        bearer
                        (pure instant)
                        ( do
                            firstRequest <- checkRead (OneArticle collection articleIdentifier)
                            secondRequest <- checkRead (OneArticle collection (articleId (storedArticle secondArticle)))

                            firstResult <- selectArticles firstRequest
                            secondResult <- selectArticles secondRequest
                            pure (firstResult, secondResult)
                        )

            assertSame ([articleFixture], [secondArticle]) result
        , testCase "unknown and add-only bearers deny before any article query" . withReadDatabase $ \_ connection _ -> do
            bearer <- requireRight (parseBearer zeroToken)
            SQL.execute_ connection "PRAGMA foreign_keys = OFF"
            SQL.execute_ connection "DROP TABLE article"
            SQL.execute_ connection "PRAGMA foreign_keys = ON"
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreDenied
            let contributor = readChild{storedGrant = (storedGrant readChild){grantScope = WholeCollection collection, grantPermissions = Set.singleton AddArticles}}
            SQL.execute_ connection "PRAGMA foreign_keys = OFF"
            insertGrant connection collection contributor
            SQL.execute_ connection "PRAGMA foreign_keys = ON"
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreDenied
            assertAutocommit connection
        , testCase "unselected malformed articles are not decoded" . withReadDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "PRAGMA ignore_check_constraints = ON"
            SQL.execute connection "UPDATE article SET title = '' WHERE id = ?" . Only =<< requireRight (toArticleBytes (articleId (storedArticle secondArticle)))
            readAt connection bearer (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture])
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreCorrupt
        , testCase "the clock is sampled inside each transaction with strict expiry" . withReadDatabase $ \_ connection _ -> do
            insertGrant connection collection readChild
            bearer <- requireRight (parseBearer zeroToken)
            calls <- newIORef (0 :: Int)

            forM_ [(fromTimestamp 1999999, True), (deadline, False), (fromTimestamp 2000001, False)] $ \(now, allowed) -> do
                let clock = do
                        Direct.getAutoCommit (connectionHandle connection) >>= (@?= False)
                        modifyIORef' calls (+ 1)
                        pure now

                result <- readWithClock clock connection bearer (OneArticle collection articleIdentifier)
                if allowed then assertSame (Right [articleFixture]) result else assertLeft StoreDenied result

            readIORef calls >>= (@?= 3)
        , testCase "four delegation edges load and validate successfully" . withReadDatabase $ \_ connection _ -> do
            installChain connection
            bearer <- requireRight (parseBearer zeroToken)
            result <- requireRight =<< readAt connection bearer (WholeCollection collection)
            sameArticles [articleFixture, secondArticle] result
        , testCase "a sixth grant is not loaded beyond the ancestry bound" . withReadDatabase $ \_ connection _ -> do
            installChain connection
            let extra = GrantSnapshot (chainId 9) (Just rootIdentifier) (WholeCollection collection) (Set.fromList [ReadArticles, Delegate]) (Just deadline) 1
            insertGrant connection collection (StoredGrant extra (ByteString.replicate 32 9) Nothing)
            identifier <- requireRight (toCapabilityBytes (chainId 9))
            SQL.execute connection "UPDATE capability SET parent_id = ? WHERE depth = 1 AND id <> ?" (identifier, identifier)
            SQL.execute_ connection "UPDATE capability SET petname = CAST(x'ff' AS TEXT) WHERE parent_id IS NULL"
            bearer <- requireRight (parseBearer zeroToken)
            await (readAt connection bearer (WholeCollection collection)) >>= assertLeft StoreDenied
        , testCase "missing, cyclic, expired, and non-attenuating ancestors deny" $
            forM_ chainCorruptions $ \mutation -> withReadDatabase $ \_ connection _ -> do
                installChain connection
                SQL.execute_ connection "PRAGMA foreign_keys = OFF"
                SQL.execute_ connection mutation
                SQL.execute_ connection "PRAGMA foreign_keys = ON"
                bearer <- requireRight (parseBearer zeroToken)
                await (readAt connection bearer (WholeCollection collection)) >>= assertLeft StoreDenied
                assertAutocommit connection
        , testCase "two files bind repeated article and grant IDs to their own bearer" . withReadDatabase $ \_ first firstBearer ->
            withEmptyDatabaseFor otherCollection $ \_ _ second -> do
                requireRight =<< initializeSchema second otherCollection
                let otherArticle = articleFixture{storedRevision = 19}
                    root = rootFixture{storedGrant = (storedGrant rootFixture){grantScope = WholeCollection otherCollection}, storedTokenHash = otherDigest}
                insertArticle second otherArticle
                insertGrant second otherCollection root
                secondBearer <- requireRight (parseBearer otherToken)

                readAt first firstBearer (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture])
                readAt second secondBearer (OneArticle otherCollection articleIdentifier) >>= assertSame (Right [otherArticle])

                readAt second firstBearer (WholeCollection collection) >>= assertLeft StoreCorrupt
                readAt first firstBearer (WholeCollection otherCollection) >>= assertLeft StoreDenied
        ]

------------------------------------------------------------------------
-- Stored state
------------------------------------------------------------------------

storageTests :: TestTree
storageTests =
    testGroup
        "Stored state"
        [ testCase "metadata and version are rechecked on every read" . withReadDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "PRAGMA user_version = 2"
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreFault
            SQL.execute connection "UPDATE collection_meta SET collection_id = ?" . Only =<< requireRight (toCollectionBytes otherCollection)
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreCorrupt
            assertAutocommit connection
        , testCase "multiple roots are corruption" . withReadDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "DROP INDEX one_root"
            insertGrant connection collection rootFixture{storedGrant = (storedGrant rootFixture){grantId = chainId 9}}
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreCorrupt
        , testCase "duplicate parent matches are corruption" . withReadDatabase $ \_ connection _ -> do
            installChain connection
            SQL.execute_ connection "PRAGMA foreign_keys = OFF"
            SQL.execute_ connection "CREATE TABLE copy AS SELECT * FROM capability"
            SQL.execute_ connection "DROP TABLE capability"
            SQL.execute_ connection "CREATE TABLE capability AS SELECT * FROM copy"
            SQL.execute_ connection "INSERT INTO capability SELECT * FROM copy WHERE depth = 2"
            SQL.execute_ connection "PRAGMA foreign_keys = ON"
            bearer <- requireRight (parseBearer zeroToken)
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreCorrupt
        , testCase "malformed parent permissions and grant fields are corruption" $
            forM_ ["UPDATE capability_permission SET permission = 'unknown' WHERE permission = 'read' AND capability_id = (SELECT id FROM capability WHERE depth = 2)", "UPDATE capability SET petname = CAST(x'ff' AS TEXT) WHERE depth = 2", "UPDATE capability SET depth = 99 WHERE depth = 2"] $ \mutation -> withReadDatabase $ \_ connection _ -> do
                installChain connection
                SQL.execute_ connection "PRAGMA ignore_check_constraints = ON"
                SQL.execute_ connection mutation
                bearer <- requireRight (parseBearer zeroToken)
                readAt connection bearer (WholeCollection collection) >>= assertLeft StoreCorrupt
                assertAutocommit connection
        , testCase "duplicate article IDs fail both full and single-article reads" . withReadDatabase $ \_ connection bearer -> do
            loosenArticles connection
            SQL.execute_ connection "INSERT INTO article SELECT * FROM copy"
            forM_ [WholeCollection collection, OneArticle collection articleIdentifier] (readAt connection bearer >=> assertLeft StoreCorrupt)
        , testCase "malformed article values, SQL types, UTF-8, and revisions fail wholly" $
            forM_ ["UPDATE article SET title = ''", "UPDATE article SET url = 'invalid'", "UPDATE article SET state = 'unknown'", "UPDATE article SET title = CAST(x'ff' AS TEXT)", "UPDATE article SET created_at = 'private stored value'", "UPDATE article SET revision = -1", "UPDATE article SET revision = 1.5", "UPDATE article SET id = x'00'"] $ \mutation -> withReadDatabase $ \_ connection bearer -> do
                loosenArticles connection
                SQL.execute_ connection "PRAGMA foreign_keys = OFF"
                SQL.execute_ connection mutation
                SQL.execute_ connection "PRAGMA foreign_keys = ON"
                readAt connection bearer (WholeCollection collection) >>= assertLeft StoreCorrupt
                assertAutocommit connection
        , testCase "missing article storage is a fault after authorization" . withReadDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "DROP TABLE article"
            readAt connection bearer (WholeCollection collection) >>= assertLeft StoreFault
            assertAutocommit connection
        ]

------------------------------------------------------------------------
-- Transaction lifetime
------------------------------------------------------------------------

transactionTests :: TestTree
transactionTests =
    testGroup
        "Transaction lifetime"
        [ testCase "an existing transaction and its writes are untouched" . withReadDatabase $ \_ connection bearer ->
            bracket_ (SQL.execute_ connection "BEGIN") (SQL.execute_ connection "ROLLBACK") $ do
                SQL.execute_ connection "UPDATE article SET revision = 42"
                readAt connection bearer (WholeCollection collection) >>= assertLeft StoreTransactionActive
                Direct.getAutoCommit (connectionHandle connection) >>= (@?= False)
                SQL.query_ connection "SELECT DISTINCT revision FROM article" >>= (@?= [Only (42 :: Int)])
        , testCase "changed connection settings are rejected without a transaction" $
            forM_ ["PRAGMA foreign_keys = OFF", "PRAGMA trusted_schema = ON", "PRAGMA busy_timeout = 100", "PRAGMA journal_mode = OFF"] $ \setting -> withReadDatabase $ \_ connection bearer -> do
                void (SQL.query_ connection setting :: IO [[SQLite.SQLData]])
                readAt connection bearer (WholeCollection collection) >>= assertLeft StoreFault
                assertAutocommit connection
        , testCase "exclusive contention returns Busy promptly and retains no transaction" . withReadDatabase $ \path connection bearer -> do
            SQL.withConnection path $ \holder -> bracket_ (SQL.execute_ holder "BEGIN EXCLUSIVE") (SQL.execute_ holder "ROLLBACK") $ do
                await (readAt connection bearer (WholeCollection collection)) >>= assertLeft StoreBusy
                assertAutocommit connection
            readAt connection bearer (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture])
        , testCase "authorization clock preserves caller masking state" . withReadDatabase $ \_ connection bearer -> do
            let check expected = do
                    let clock = getMaskingState >>= (@?= expected) >> pure instant
                    result <- readWithClock clock connection bearer (OneArticle collection articleIdentifier)
                    assertSame (Right [articleFixture]) result
                    assertAutocommit connection

            check Unmasked
            mask_ (check MaskedInterruptible)
        , testCase "unexpected clock exceptions propagate after cleanup" . withReadDatabase $ \_ connection bearer -> do
            result <- try @CallbackFailure (readWithClock (throwIO CallbackFailure) connection bearer (WholeCollection collection))
            case result of
                Left CallbackFailure -> pure ()
                Right _ -> assertFailure "unexpected exception was swallowed"
            assertAutocommit connection
        , testCase "cancellation during authorization releases the transaction" . withReadDatabase $ \path connection bearer -> do
            entered <- newEmptyMVar
            blocked <- newEmptyMVar @()
            finished <- newEmptyMVar
            let clock = putMVar entered () >> takeMVar blocked >> pure instant

            bracket (forkFinally (readWithClock clock connection bearer (WholeCollection collection)) (putMVar finished)) killThread $ \worker -> do
                await (takeMVar entered)
                killThread worker

                result <- await (takeMVar finished)
                case result of
                    Left exception -> fromException exception @?= Just ThreadKilled
                    Right _ -> assertFailure "cancellation was swallowed"

            assertAutocommit connection
            requireRight =<< withExistingFile path (`SQL.execute_` "UPDATE article SET revision = revision + 1")
        , testCase "COMMIT Busy rolls back a controlled clock-side write" . withReadDatabase $ \path connection bearer -> do
            requireRight
                =<< withExistingFile
                    path
                    ( \reader -> bracket_ (SQL.execute_ reader "BEGIN") (SQL.execute_ reader "ROLLBACK") $ do
                        void (SQL.query_ reader "SELECT id FROM article" :: IO [Only ByteString.ByteString])
                        let clock = SQL.execute_ connection "UPDATE article SET revision = 42" >> pure instant

                        await (readWithClock clock connection bearer (WholeCollection collection)) >>= assertLeft StoreBusy
                        assertAutocommit connection
                    )
            readAt connection bearer (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture])
        , testCase "SQLite automatic rollback preserves the original fault" . withReadDatabase $ \_ connection bearer -> do
            let clock = do
                    SQL.execute_ connection "UPDATE article SET revision = 42"
                    SQL.execute_ connection "INSERT OR ROLLBACK INTO article SELECT * FROM article"
                    pure instant

            readWithClock clock connection bearer (WholeCollection collection) >>= assertLeft StoreFault
            assertAutocommit connection
            readAt connection bearer (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture])
        , testCase "a read finishes before a waiting revocation commits; the next read is denied" . withReadDatabase $ \path connection _ -> do
            insertGrant connection collection readChild
            bearer <- requireRight (parseBearer zeroToken)
            entered <- newEmptyMVar
            resume <- newEmptyMVar
            finished <- newEmptyMVar
            let clock = putMVar entered () >> takeMVar resume >> pure instant

            bracket (forkFinally (readWithClock clock connection bearer (OneArticle collection articleIdentifier)) (putMVar finished)) killThread $ \_ -> do
                await (takeMVar entered)
                requireRight
                    =<< withExistingFile
                        path
                        ( \writer -> do
                            SQL.execute_ writer "BEGIN IMMEDIATE"
                            SQL.execute_ writer "UPDATE article SET revision = 42"
                            SQL.execute writer "DELETE FROM capability WHERE id = ?" . Only =<< requireRight (toCapabilityBytes childIdentifier)

                            commit <- try @SQL.SQLError (SQL.execute_ writer "COMMIT")
                            case commit of
                                Left err -> SQL.sqlError err @?= SQLite.ErrorBusy
                                Right () -> assertFailure "writer committed while the read transaction was active"

                            putMVar resume ()
                            result <- await (takeMVar finished)
                            case result of
                                Left exception -> throwIO exception
                                Right articles -> assertSame (Right [articleFixture]) articles
                            SQL.execute_ writer "COMMIT"
                        )

            readAt connection bearer (OneArticle collection articleIdentifier) >>= assertLeft StoreDenied
            owner <- requireRight (parseBearer goldenToken)
            readAt connection owner (OneArticle collection articleIdentifier) >>= assertSame (Right [articleFixture{storedRevision = 42}])
            assertAutocommit connection
        ]

------------------------------------------------------------------------
-- Fixtures and database checks
------------------------------------------------------------------------

instant, deadline :: UTCTime
instant = fromTimestamp 1000000
deadline = fromTimestamp 2000000

readChild :: StoredGrant
readChild = childFixture{storedGrant = (storedGrant childFixture){grantExpiresAt = Just deadline}, storedTokenHash = zeroDigest}

secondArticle :: StoredArticle
secondArticle = articleFixture{storedArticle = (storedArticle articleFixture){articleId = identifier, articleUrl = secondUrl}, storedRevision = 7}
  where
    identifier = either (error . show) id (parseArticleId "123e4567-e89b-42d3-a456-426614174008")

withReadDatabase :: (FilePath -> Connection -> Bearer -> IO a) -> IO a
withReadDatabase use = withEmptyDatabase $ \path _ connection -> do
    requireRight =<< initializeSchema connection collection
    insertGrant connection collection rootFixture{storedTokenHash = goldenDigest}
    insertArticle connection articleFixture
    insertArticle connection secondArticle
    bearer <- requireRight (parseBearer goldenToken)
    use path connection bearer

readAt :: Connection -> Bearer -> Scope -> IO (Either StoreError [StoredArticle])
readAt = readWithClock (pure instant)

readWithClock :: IO UTCTime -> Connection -> Bearer -> Scope -> IO (Either StoreError [StoredArticle])
readWithClock clock connection bearer = readArticles connection bearer clock

assertAutocommit :: Connection -> IO ()
assertAutocommit connection = Direct.getAutoCommit (connectionHandle connection) >>= (@?= True)

sameArticles :: [StoredArticle] -> [StoredArticle] -> IO ()
sameArticles expected actual = assertSame (sortOn (articleId . storedArticle) expected) (sortOn (articleId . storedArticle) actual)

chainId :: Int -> CapabilityId
chainId depth = either (error . show) id . parseCapabilityId $ "123e4567-e89b-42d3-a456-42661417401" <> Text.pack (show depth)

installChain :: Connection -> IO ()
installChain connection = forM_ [1 .. 4] $ \depth -> do
    let parent = if depth == 1 then rootIdentifier else chainId (depth - 1)
        grant = GrantSnapshot (chainId depth) (Just parent) (WholeCollection collection) (Set.fromList [ReadArticles, Delegate]) (Just deadline) depth
        digest = if depth == 4 then zeroDigest else ByteString.replicate 32 (fromIntegral depth)
    insertGrant connection collection (StoredGrant grant digest Nothing)

chainCorruptions :: [SQL.Query]
chainCorruptions =
    [ "DELETE FROM capability WHERE depth = 2"
    , "UPDATE capability SET parent_id = (SELECT id FROM capability WHERE depth = 4) WHERE depth = 2"
    , "UPDATE capability SET depth = 1 WHERE depth = 2"
    , "DELETE FROM capability_permission WHERE permission = 'delegate' AND capability_id = (SELECT id FROM capability WHERE depth = 2)"
    , "DELETE FROM capability_permission WHERE permission = 'read' AND capability_id = (SELECT id FROM capability WHERE depth = 2)"
    , "UPDATE capability SET expires_at = 1000000 WHERE depth = 2"
    , "UPDATE capability SET expires_at = 1500000 WHERE depth = 2"
    , "UPDATE capability SET expires_at = 1000000 WHERE depth = 4"
    , "UPDATE capability SET scope_article_id = (SELECT id FROM article LIMIT 1) WHERE depth = 2"
    ]

loosenArticles :: Connection -> IO ()
loosenArticles connection = do
    SQL.execute_ connection "PRAGMA foreign_keys = OFF"
    SQL.execute_ connection "CREATE TABLE copy AS SELECT * FROM article"
    SQL.execute_ connection "DROP TABLE article"
    SQL.execute_ connection "CREATE TABLE article AS SELECT * FROM copy"
    SQL.execute_ connection "PRAGMA foreign_keys = ON"

-- | A URL distinct from the first article's, because a collection stores each URL once.
secondUrl :: WebUrl
secondUrl = either (error . show) id (parseWebUrl "https://example.com/second")
