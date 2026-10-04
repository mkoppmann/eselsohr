{- |
Module      : TitleTests
Description : Protected title change and revision tests

'titleTests' covers stored authority, sealed request inputs, and title edits with expected revisions.
Cases check conflicts, matching no-ops, counter exhaustion, and preservation of other article fields.

Concurrency and failure tests check writer ordering, revocation, rollback after an earlier update, cancellation, and the caller's exception masking state.
-}
module TitleTests (titleTests) where

import BearerTests (goldenDigest, goldenToken, otherDigest, otherToken, zeroDigest, zeroToken)
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), MaskingState (..), bracket, bracket_, fromException, getMaskingState, mask_, throwIO, try)
import Control.Monad (forM_, void, when, (>=>))
import Data.ByteString (ByteString)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Article
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

titleTests :: TestTree
titleTests = testGroup "Protected title change" [authorityTests, revisionTests, transactionTests]

------------------------------------------------------------------------
-- Authority and sealed payload
------------------------------------------------------------------------

authorityTests :: TestTree
authorityTests =
    testGroup
        "Authority and sealed payload"
        [ testCase "named owner change commits exact title and preserves other fields and articles" . withTitleDatabase $ \path connection bearer -> do
            title <- requireRight (mkTitle "quotes ' \" ; -- ? & # é 😀")
            let expected = renamed title 1 articleFixture

            changeTitle connection bearer (pure instant) articleIdentifier title 0 >>= assertSame (Right expected)
            assertStored connection expected
            assertStored connection secondArticle
            requireRight =<< withExistingFile path (`assertStored` expected)
            assertAutocommit connection
        , testCase "all Read and ChangeTitles combinations enforce both permissions including no-ops" $
            forM_ [[], [ReadArticles], [ChangeTitles], [ReadArticles, ChangeTitles], [AddArticles]] $ \permissions ->
                withTitleDatabase $ \_ connection _ -> do
                    let grant = titleChild{storedGrant = (storedGrant titleChild){grantPermissions = Set.fromList permissions}}
                    insertGrant connection collection grant
                    bearer <- requireRight (parseBearer zeroToken)

                    forM_ [articleTitle (storedArticle articleFixture), newTitle] $ \title -> do
                        result <- writeAt connection bearer articleIdentifier title 0
                        if ReadArticles `elem` permissions && ChangeTitles `elem` permissions
                            then assertSame (Right (renamed title (if title == newTitle then 1 else 0) articleFixture)) result
                            else assertLeft StoreDenied result
                    assertAutocommit connection
        , testCase "article-scoped authority cannot change a different article" . withTitleDatabase $ \_ connection _ -> do
            bearer <- installChild connection
            writeAt connection bearer secondIdentifier newTitle 7 >>= assertLeft StoreDenied
            assertStored connection secondArticle
        , testCase "unknown and add-only bearers deny before article reads and revision checks" . withTitleDatabase $ \_ connection _ -> do
            bearer <- requireRight (parseBearer zeroToken)
            SQL.execute_ connection "PRAGMA foreign_keys = OFF"
            SQL.execute_ connection "DROP TABLE article"
            SQL.execute_ connection "PRAGMA foreign_keys = ON"
            forM_ [-1, 0, 1, limit + 1] (writeAt connection bearer articleIdentifier newTitle >=> assertLeft StoreDenied)

            let grant = titleChild{storedGrant = (storedGrant titleChild){grantScope = WholeCollection collection, grantPermissions = Set.singleton AddArticles}}
            SQL.execute_ connection "PRAGMA foreign_keys = OFF"
            insertGrant connection collection grant
            SQL.execute_ connection "PRAGMA foreign_keys = ON"
            forM_ [-1, 0, 1, limit + 1] (writeAt connection bearer articleIdentifier newTitle >=> assertLeft StoreDenied)
            assertAutocommit connection
        , testCase "seals retain independent article IDs and titles after another check" . withTitleDatabase $ \_ connection bearer -> do
            result <-
                requireRight
                    =<< runWriteTransaction
                        connection
                        bearer
                        (pure instant)
                        ( do
                            first <- checkTitleChange articleIdentifier newTitle 0
                            second <- checkTitleChange secondIdentifier otherTitle 7

                            firstResult <- writeTitle first
                            secondResult <- writeTitle second
                            pure (firstResult, secondResult)
                        )

            let expected = (renamed newTitle 1 articleFixture, renamed otherTitle 8 secondArticle)
            assertSame expected result
            assertStored connection (fst expected)
            assertStored connection (snd expected)
        , testCase "checking another request does not replace the earlier request's expected revision" . withTitleDatabase $ \_ connection bearer -> do
            result <-
                runWriteTransaction
                    connection
                    bearer
                    (pure instant)
                    ( do
                        stale <- checkTitleChange articleIdentifier newTitle 1
                        _ <- checkTitleChange articleIdentifier newTitle 0
                        writeTitle stale
                    )

            assertLeft StoreConflict result
            assertStored connection articleFixture
        , testCase "executing the same title-change request twice conflicts and rolls back" . withTitleDatabase $ \_ connection bearer -> do
            result <-
                runWriteTransaction
                    connection
                    bearer
                    (pure instant)
                    ( do
                        request <- checkTitleChange articleIdentifier newTitle 0
                        _ <- writeTitle request
                        writeTitle request
                    )

            assertLeft StoreConflict result
            assertStored connection articleFixture
        , testCase "fresh time is sampled once inside every write with strict expiration" . withTitleDatabase $ \_ connection _ -> do
            bearer <- installChild connection
            calls <- newIORef (0 :: Int)

            forM_ [(fromTimestamp 1999999, True), (deadline, False), (fromTimestamp 2000001, False)] $ \(now, allowed) -> do
                let clock = do
                        Direct.getAutoCommit (connectionHandle connection) >>= (@?= False)
                        modifyIORef' calls (+ 1)
                        pure now

                result <- writeWithClock clock connection bearer articleIdentifier (articleTitle (storedArticle articleFixture)) 0
                if allowed then assertSame (Right articleFixture) result else assertLeft StoreDenied result

            readIORef calls >>= (@?= 3)
        , testCase "expired or revoked ancestors deny a still-live child" $
            forM_ ["UPDATE capability SET expires_at = 1000000 WHERE depth = 1", "DELETE FROM capability WHERE depth = 1"] $ \mutation ->
                withTitleDatabase $ \_ connection _ -> do
                    let parent = titleChild{storedGrant = (storedGrant titleChild){grantScope = WholeCollection collection, grantPermissions = Set.fromList [ReadArticles, ChangeTitles, Delegate]}, storedTokenHash = otherDigest}
                        child = titleChild{storedGrant = (storedGrant titleChild){grantId = grandchildIdentifier, grantParentId = Just childIdentifier, grantDepth = 2}}
                    insertGrant connection collection parent
                    insertGrant connection collection child
                    bearer <- requireRight (parseBearer zeroToken)
                    SQL.execute_ connection mutation

                    writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreDenied
                    assertStored connection articleFixture
        , testCase "identity and version are rechecked inside every write" . withTitleDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "PRAGMA user_version = 2"
            writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreFault
            SQL.execute connection "UPDATE collection_meta SET collection_id = ?" . Only =<< requireRight (toCollectionBytes otherCollection)
            writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreCorrupt
            assertStored connection articleFixture
        , testCase "two files bind repeated IDs and revision counters to their own bearer" . withTitleDatabase $ \_ first firstBearer ->
            withEmptyDatabaseFor otherCollection $ \_ _ second -> do
                requireRight =<< initializeSchema second otherCollection
                insertArticle second articleFixture{storedRevision = 19}
                insertGrant second otherCollection rootFixture{storedGrant = (storedGrant rootFixture){grantScope = WholeCollection otherCollection}, storedTokenHash = otherDigest}
                secondBearer <- requireRight (parseBearer otherToken)

                writeAt second firstBearer articleIdentifier newTitle 19 >>= assertLeft StoreCorrupt
                writeAt first secondBearer articleIdentifier newTitle 0 >>= assertLeft StoreCorrupt
                writeAt first firstBearer articleIdentifier newTitle 0 >>= assertSame (Right (renamed newTitle 1 articleFixture))
                writeAt second secondBearer articleIdentifier otherTitle 19 >>= assertSame (Right (renamed otherTitle 20 articleFixture))

                assertStored first (renamed newTitle 1 articleFixture)
                assertStored second (renamed otherTitle 20 articleFixture)
        ]

------------------------------------------------------------------------
-- Revision and stored state
------------------------------------------------------------------------

revisionTests :: TestTree
revisionTests =
    testGroup
        "Revision and stored state"
        [ testCase "older and newer expected revisions conflict for changes and no-ops" . withTitleDatabase $ \_ connection bearer -> do
            forM_ [6, 8] $ \revision -> forM_ [newTitle, articleTitle (storedArticle secondArticle)] $ \title -> do
                writeAt connection bearer secondIdentifier title revision >>= assertLeft StoreConflict
                assertStored connection secondArticle
        , testCase "negative and oversized submitted revisions fail without narrowing" . withTitleDatabase $ \_ connection bearer -> do
            forM_ [-1, limit + 1, 2 ^ (100 :: Int)] (writeAt connection bearer articleIdentifier newTitle >=> assertLeft StoreInvalidRevision)
            assertStored connection articleFixture
        , testCase "matching no-ops perform no UPDATE even at maximum revision" . withTitleDatabase $ \_ connection bearer -> do
            forM_ [0, limit] $ \revision -> do
                SQL.execute connection "UPDATE article SET revision = ? WHERE id = ?" (revision, articleBytes)
                before <- totalChanges connection
                let expected = articleFixture{storedRevision = revision}

                writeAt connection bearer articleIdentifier (articleTitle (storedArticle articleFixture)) revision >>= assertSame (Right expected)
                totalChanges connection >>= (@?= before)
                assertStored connection expected
        , testCase "committed changes increment once and counter exhaustion leaves the last title intact" . withTitleDatabase $ \_ connection bearer -> do
            SQL.execute connection "UPDATE article SET revision = ? WHERE id = ?" (limit - 1, articleBytes)
            let expected = renamed newTitle limit articleFixture

            writeAt connection bearer articleIdentifier newTitle (limit - 1) >>= assertSame (Right expected)
            writeAt connection bearer articleIdentifier otherTitle limit >>= assertLeft StoreRevisionExhausted
            assertStored connection expected
        , testCase "authorized missing article returns NotFound" . withTitleDatabase $ \_ connection bearer -> do
            writeAt connection bearer missingIdentifier newTitle 0 >>= assertLeft StoreNotFound
            assertStored connection articleFixture
        , testCase "malformed selected values and SQL types fail before writing" $
            forM_ ["title = ''", "title = CAST(x'ff' AS TEXT)", "revision = -1", "revision = 'bad'", "revision = 9223372036854775808", "created_at = NULL"] $ \assignment ->
                withTitleDatabase $ \_ connection bearer -> do
                    loosenArticles connection
                    SQL.execute connection ("UPDATE article SET " <> assignment <> " WHERE id = ?") (Only articleBytes)
                    before <- totalChanges connection

                    writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreCorrupt
                    totalChanges connection >>= (@?= before)
                    assertAutocommit connection
        , testCase "duplicate target rows are corruption without an UPDATE" . withTitleDatabase $ \_ connection bearer -> do
            loosenArticles connection
            insertArticle connection articleFixture
            before <- totalChanges connection

            writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreCorrupt
            totalChanges connection >>= (@?= before)
        , testCase "unselected malformed article is not decoded or changed" . withTitleDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "PRAGMA ignore_check_constraints = ON"
            SQL.execute connection "UPDATE article SET title = '' WHERE id = ?" . Only =<< requireRight (toArticleBytes secondIdentifier)
            writeAt connection bearer articleIdentifier newTitle 0 >>= assertSame (Right (renamed newTitle 1 articleFixture))
            identifier <- requireRight (toArticleBytes secondIdentifier)
            SQL.query connection "SELECT title, revision FROM article WHERE id = ?" (Only identifier) >>= (@?= [("" :: Text, 7 :: Int64)])
        , testCase "unexpected affected row count is corruption and rolls back earlier writes" . withTitleDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "CREATE TRIGGER ignore_title BEFORE UPDATE ON article WHEN OLD.revision = 7 BEGIN SELECT RAISE(IGNORE); END"
            result <-
                runWriteTransaction
                    connection
                    bearer
                    (pure instant)
                    ( do
                        _ <- checkTitleChange articleIdentifier newTitle 0 >>= writeTitle
                        checkTitleChange secondIdentifier newTitle 7 >>= writeTitle
                    )

            assertLeft StoreCorrupt result
            assertStored connection articleFixture
            assertStored connection secondArticle
        ]

------------------------------------------------------------------------
-- Write transaction lifetime and ordering
------------------------------------------------------------------------

transactionTests :: TestTree
transactionTests =
    testGroup
        "Write transaction lifetime and ordering"
        [ testCase "write settings are configured and read back before authorization" . withTitleDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "PRAGMA synchronous = OFF"
            SQL.execute_ connection "PRAGMA secure_delete = OFF"
            let clock = do
                    settings connection >>= (@?= [3, 1])
                    SQL.query_ connection "PRAGMA main.journal_mode" >>= (@?= [Only ("delete" :: Text)])
                    pure instant

            writeWithClock clock connection bearer articleIdentifier newTitle 0 >>= assertSame (Right (renamed newTitle 1 articleFixture))
            settings connection >>= (@?= [3, 1])
        , testCase "existing caller transaction and settings are unchanged" . withTitleDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "PRAGMA synchronous = FULL"
            SQL.execute_ connection "PRAGMA secure_delete = OFF"

            bracket_ (SQL.execute_ connection "BEGIN IMMEDIATE") (SQL.execute_ connection "ROLLBACK") $ do
                SQL.execute_ connection "UPDATE article SET revision = 21"
                writeAt connection bearer articleIdentifier newTitle 21 >>= assertLeft StoreTransactionActive
                settings connection >>= (@?= [2, 0])
                Direct.getAutoCommit (connectionHandle connection) >>= (@?= False)
                assertStored connection articleFixture{storedRevision = 21}

            assertStored connection articleFixture
        , testCase "unsupported journal modes are rejected without conversion or configuration changes" $
            forM_ ["TRUNCATE", "PERSIST", "MEMORY", "WAL", "OFF"] $ \mode -> withTitleDatabase $ \_ connection bearer -> do
                void (SQL.query_ connection ("PRAGMA journal_mode = " <> mode) :: IO [Only Text])
                before <- SQL.query_ connection "PRAGMA journal_mode" :: IO [Only Text]
                SQL.execute_ connection "PRAGMA synchronous = FULL"
                SQL.execute_ connection "PRAGMA secure_delete = OFF"
                writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreFault
                SQL.query_ connection "PRAGMA journal_mode" >>= (@?= before)
                settings connection >>= (@?= [2, 0])
                assertStored connection articleFixture
                assertAutocommit connection
        , testCase "required opener settings are checked before changing write settings" $
            forM_ ["PRAGMA foreign_keys = OFF", "PRAGMA trusted_schema = ON", "PRAGMA busy_timeout = 1"] $ \setting -> withTitleDatabase $ \_ connection bearer -> do
                SQL.execute_ connection setting
                SQL.execute_ connection "PRAGMA synchronous = FULL"
                SQL.execute_ connection "PRAGMA secure_delete = OFF"
                writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreFault
                settings connection >>= (@?= [2, 0])
                assertStored connection articleFixture
        , testCase "a later conflict rolls back an earlier title and revision change" . withTitleDatabase $ \_ connection bearer -> do
            result <-
                runWriteTransaction
                    connection
                    bearer
                    (pure instant)
                    ( do
                        _ <- checkTitleChange articleIdentifier newTitle 0 >>= writeTitle
                        checkTitleChange secondIdentifier newTitle 6 >>= writeTitle
                    )

            assertLeft StoreConflict result
            assertStored connection articleFixture
            assertStored connection secondArticle
            assertAutocommit connection
        , testCase "authorization clock preserves caller masking state" . withTitleDatabase $ \_ connection bearer -> do
            let check expected revision title = do
                    let clock = getMaskingState >>= (@?= expected) >> pure instant
                    result <- writeWithClock clock connection bearer articleIdentifier title revision
                    let changed = renamed title (revision + 1) articleFixture
                    assertSame (Right changed) result
                    assertStored connection changed
                    assertAutocommit connection

            check Unmasked 0 newTitle
            mask_ (check MaskedInterruptible 1 otherTitle)
        , testCase "unexpected exception after an UPDATE propagates after rollback" . withTitleDatabase $ \_ connection bearer -> do
            calls <- newIORef (0 :: Int)
            let clock = do
                    modifyIORef' calls (+ 1)
                    count <- readIORef calls
                    when (count == 2) (throwIO CallbackFailure)
                    pure instant

            result <-
                try @CallbackFailure $
                    runWriteTransaction
                        connection
                        bearer
                        clock
                        ( do
                            _ <- checkTitleChange articleIdentifier newTitle 0 >>= writeTitle
                            checkTitleChange secondIdentifier newTitle 7 >>= writeTitle
                        )

            case result of
                Left exception -> exception @?= CallbackFailure
                Right _ -> assertFailure "exception was swallowed"
            assertStored connection articleFixture
            assertAutocommit connection
        , testCase "cancellation after an UPDATE rolls back and releases the write lock" . withTitleDatabase $ \path connection bearer -> do
            entered <- newEmptyMVar
            blocked <- newEmptyMVar @()
            finished <- newEmptyMVar
            calls <- newIORef (0 :: Int)
            let clock = do
                    modifyIORef' calls (+ 1)
                    count <- readIORef calls
                    when (count == 2) (putMVar entered () >> takeMVar blocked)
                    pure instant
                action =
                    runWriteTransaction
                        connection
                        bearer
                        clock
                        ( do
                            _ <- checkTitleChange articleIdentifier newTitle 0 >>= writeTitle
                            checkTitleChange secondIdentifier newTitle 7 >>= writeTitle
                        )

            bracket (forkFinally action (putMVar finished)) killThread $ \thread -> do
                await (takeMVar entered)
                killThread thread

                outcome <- await (takeMVar finished)
                case outcome of
                    Left exception -> fromException exception @?= Just ThreadKilled
                    Right _ -> assertFailure "cancellation was swallowed"

            assertStored connection articleFixture
            assertAutocommit connection
            requireRight =<< withExistingFile path (\other -> bracket_ (SQL.execute_ other "BEGIN IMMEDIATE") (SQL.execute_ other "ROLLBACK") (pure ()))
        , testCase "COMMIT Busy rolls back the attempted title and revision change" . withTitleDatabase $ \path connection bearer ->
            void $
                requireRight
                    =<< withExistingFile
                        path
                        ( \reader -> bracket_ (SQL.execute_ reader "BEGIN DEFERRED") (SQL.execute_ reader "ROLLBACK") $ do
                            assertStored reader articleFixture
                            await (writeAt connection bearer articleIdentifier newTitle 0) >>= assertLeft StoreBusy
                            assertStored connection articleFixture
                            assertAutocommit connection
                        )
        , testCase "SQLite automatic rollback after an UPDATE preserves the original state" . withTitleDatabase $ \_ connection bearer -> do
            SQL.execute_ connection "CREATE TRIGGER reject_title BEFORE UPDATE ON article WHEN OLD.revision = 7 BEGIN SELECT RAISE(ROLLBACK, 'fixture'); END"
            result <-
                runWriteTransaction
                    connection
                    bearer
                    (pure instant)
                    ( do
                        _ <- checkTitleChange articleIdentifier newTitle 0 >>= writeTitle
                        checkTitleChange secondIdentifier newTitle 7 >>= writeTitle
                    )

            assertLeft StoreFault result
            assertStored connection articleFixture
            assertAutocommit connection
        , testCase "competing edits serialize and a stale retry conflicts" . withTitleDatabase $ \path connection bearer -> do
            heldWrite connection bearer $ \resume finish -> do
                requireRight
                    =<< withExistingFile
                        path
                        ( \other -> do
                            await (writeAt other bearer articleIdentifier otherTitle 0) >>= assertLeft StoreBusy
                            assertAutocommit other
                        )

                resume
                finish >>= assertSame (Right (renamed newTitle 1 articleFixture))

            writeAt connection bearer articleIdentifier otherTitle 0 >>= assertLeft StoreConflict
            assertStored connection (renamed newTitle 1 articleFixture)
        , testCase "a write ordered before ancestor revocation may finish; later writes deny" . withTitleDatabase $ \path connection _ -> do
            bearer <- installGrandchild connection
            heldWrite connection bearer $ \resume finish -> do
                requireRight
                    =<< withExistingFile
                        path
                        ( \revoker -> do
                            blocked <- try @SQL.SQLError (SQL.execute_ revoker "BEGIN IMMEDIATE")
                            case blocked of
                                Left err -> SQL.sqlError err @?= SQLite.ErrorBusy
                                Right () -> assertFailure "revoker acquired the held writer lock"

                            resume
                            finish >>= assertSame (Right (renamed newTitle 1 articleFixture))
                            SQL.withImmediateTransaction revoker (revokeAncestor revoker)
                        )

            writeAt connection bearer articleIdentifier otherTitle 1 >>= assertLeft StoreDenied
            assertStored connection (renamed newTitle 1 articleFixture)
        , testCase "committed ancestor revocation wins over a waiting edit" . withTitleDatabase $ \path connection _ -> do
            bearer <- installGrandchild connection
            requireRight
                =<< withExistingFile
                    path
                    ( \revoker -> do
                        SQL.execute_ revoker "BEGIN IMMEDIATE"
                        revokeAncestor revoker
                        await (writeAt connection bearer articleIdentifier newTitle 0) >>= assertLeft StoreBusy
                        SQL.execute_ revoker "COMMIT"
                    )

            writeAt connection bearer articleIdentifier newTitle 0 >>= assertLeft StoreDenied
            assertStored connection articleFixture
        ]

------------------------------------------------------------------------
-- Fixtures and database checks
------------------------------------------------------------------------

instant, deadline :: UTCTime
instant = fromTimestamp 1000000
deadline = fromTimestamp 2000000

limit :: Integer
limit = toInteger (maxBound :: Int64)

newTitle, otherTitle :: Title
newTitle = either (error . show) id (mkTitle "Changed title")
otherTitle = either (error . show) id (mkTitle "Other title")

secondIdentifier, missingIdentifier :: ArticleId
secondIdentifier = either (error . show) id (parseArticleId "123e4567-e89b-42d3-a456-426614174008")
missingIdentifier = either (error . show) id (parseArticleId "123e4567-e89b-42d3-a456-426614174099")

grandchildIdentifier :: CapabilityId
grandchildIdentifier = either (error . show) id (parseCapabilityId "123e4567-e89b-42d3-a456-426614174009")

articleBytes :: ByteString
articleBytes = either (error . show) id (toArticleBytes articleIdentifier)

secondArticle :: StoredArticle
secondArticle = articleFixture{storedArticle = (storedArticle articleFixture){articleId = secondIdentifier, articleUrl = secondUrl}, storedRevision = 7}

titleChild :: StoredGrant
titleChild = childFixture{storedGrant = (storedGrant childFixture){grantExpiresAt = Just deadline, grantPermissions = Set.fromList [ReadArticles, ChangeTitles]}, storedTokenHash = zeroDigest}

renamed :: Title -> Integer -> StoredArticle -> StoredArticle
renamed title revision current = StoredArticle (rename title (storedArticle current)) revision

withTitleDatabase :: (FilePath -> Connection -> Bearer -> IO a) -> IO a
withTitleDatabase use = withEmptyDatabase $ \path _ connection -> do
    requireRight =<< initializeSchema connection collection
    insertGrant connection collection rootFixture{storedTokenHash = goldenDigest}
    insertArticle connection articleFixture
    insertArticle connection secondArticle
    bearer <- requireRight (parseBearer goldenToken)
    use path connection bearer

installChild :: Connection -> IO Bearer
installChild connection = do
    insertGrant connection collection titleChild
    requireRight (parseBearer zeroToken)

installGrandchild :: Connection -> IO Bearer
installGrandchild connection = do
    insertGrant connection collection titleChild{storedGrant = (storedGrant titleChild){grantScope = WholeCollection collection, grantPermissions = Set.fromList [ReadArticles, ChangeTitles, Delegate]}, storedTokenHash = otherDigest}
    insertGrant connection collection titleChild{storedGrant = (storedGrant titleChild){grantId = grandchildIdentifier, grantParentId = Just childIdentifier, grantDepth = 2}}
    requireRight (parseBearer zeroToken)

revokeAncestor :: Connection -> IO ()
revokeAncestor connection = SQL.execute connection "DELETE FROM capability WHERE id = ?" . Only =<< requireRight (toCapabilityBytes childIdentifier)

writeAt :: Connection -> Bearer -> ArticleId -> Title -> Integer -> IO (Either StoreError StoredArticle)
writeAt = writeWithClock (pure instant)

writeWithClock :: IO UTCTime -> Connection -> Bearer -> ArticleId -> Title -> Integer -> IO (Either StoreError StoredArticle)
writeWithClock clock connection bearer = changeTitle connection bearer clock

assertStored :: Connection -> StoredArticle -> IO ()
assertStored connection expected = do
    identifier <- requireRight . toArticleBytes . articleId $ storedArticle expected
    rows <- SQL.query connection "SELECT id, title, url, state, created_at, revision FROM article WHERE id = ?" (Only identifier)
    articles <- requireRight (traverse fromArticleRow rows)
    assertSame [expected] articles

assertAutocommit :: Connection -> IO ()
assertAutocommit connection = Direct.getAutoCommit (connectionHandle connection) >>= (@?= True)

settings :: Connection -> IO [Int64]
settings connection = do
    values <- mapM (SQL.query_ connection) ["PRAGMA main.synchronous", "PRAGMA main.secure_delete"]
    pure [value | [Only value] <- values]

totalChanges :: Connection -> IO [Only Int64]
totalChanges connection = SQL.query_ connection "SELECT total_changes()"

loosenArticles :: Connection -> IO ()
loosenArticles connection = do
    SQL.execute_ connection "PRAGMA foreign_keys = OFF"
    SQL.execute_ connection "CREATE TABLE copy AS SELECT * FROM article"
    SQL.execute_ connection "DROP TABLE article"
    SQL.execute_ connection "CREATE TABLE article AS SELECT * FROM copy"
    SQL.execute_ connection "PRAGMA foreign_keys = ON"

heldWrite :: Connection -> Bearer -> (IO () -> IO (Either StoreError StoredArticle) -> IO a) -> IO a
heldWrite connection bearer use = do
    entered <- newEmptyMVar
    resume <- newEmptyMVar
    finished <- newEmptyMVar
    let clock = putMVar entered () >> takeMVar resume >> pure instant
        action = writeWithClock clock connection bearer articleIdentifier newTitle 0

    bracket (forkFinally action (putMVar finished)) killThread $ \_ -> do
        await (takeMVar entered)
        use (putMVar resume ()) (await (takeMVar finished) >>= either throwIO pure)

-- | A URL distinct from the first article's, because a collection stores each URL once.
secondUrl :: WebUrl
secondUrl = either (error . show) id (parseWebUrl "https://example.com/second")
