{- |
Module      : RuntimeTests
Description : Store binding, runner, and result translation tests

'runtimeTests' checks every row of the translation from opening errors and protected-operation results to the application-facing read and edit results.
Missing collections and denied bearers must become denials, and every other storage failure a fault.

Runner tests use real collection files in a temporary directory and bindings with controlled clocks.
They check that each operation acts only on the bearer's collection, reloads authority on every call, samples the clock inside its transaction, and opens the collection anew each time.
Runner tests also describe the bound grant and name its collection.
Presented-bearer tests check that 'bindPresented' binds a created owner bearer and rejects malformed and oversized text, and that 'resolveDirectory' rejects paths that are not directories.
'retitleArticle' is shaped like an application function: its type names only the read and write effects, and the tests run it through the production runners with a test binding.

Cleanup tests throw from and cancel at the bound clock, while the write transaction holds the collection's write lock, and check that nothing changes, the lock is released, and the exception propagates.
-}
module RuntimeTests (runtimeTests) where

import BearerTests (goldenDigest, goldenToken, otherDigest, otherToken, zeroDigest, zeroToken)
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), bracket, bracket_, fromException, throwIO, try)
import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection, Only (..), SQLError (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Effectful (Eff, IOE, runEff, (:>))
import Eselsohr.Article (Article (..), Title, mkTitle, rename)
import Eselsohr.Capability
import Eselsohr.Id
import Eselsohr.Store
import Eselsohr.Store.Runtime
import Eselsohr.Store.SQLite.Articles (StoreError (..))
import Eselsohr.Store.SQLite.Bearer (parseBearer, renderBearer)
import Eselsohr.Store.SQLite.Codec (StoredArticle (..), StoredGrant (..), fromTimestamp, toCapabilityBytes)
import Eselsohr.Store.SQLite.Create qualified as Create
import Eselsohr.Store.SQLite.Open (OpenError, mkCollectionDirectory, withExistingFile)
import Eselsohr.Store.SQLite.Open qualified as Open
import Eselsohr.Store.SQLite.Schema (initializeSchema)
import Eselsohr.WebUrl (WebUrl, parseWebUrl)
import SqliteFixtures
import System.Directory (listDirectory, renameFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

runtimeTests :: TestTree
runtimeTests = testGroup "Store runtime" [translationTests, runnerTests, presentedTests, applicationTests, cleanupTests]

------------------------------------------------------------------------
-- Result translation
------------------------------------------------------------------------

translationTests :: TestTree
translationTests =
    testGroup
        "Result translation"
        [ testCase "opening errors become denials or faults" . forM_ openingRows $ \(err, readExpected, editExpected) -> do
            assertLeft readExpected (readResult (Left err))
            describeResult (Left err) @?= Left readExpected
            assertLeft editExpected (editResult (Left err))
        , testCase "operation errors become denials, edit failures, or faults" . forM_ operationRows $ \(err, readExpected, editExpected) -> do
            assertLeft readExpected (readResult (Right (Left err)))
            describeResult (Right (Left err)) @?= Left readExpected
            assertLeft editExpected (editResult (Right (Left err)))
        , testCase "addition errors become denials or faults" $ do
            forM_ openingRows $ \(err, _, editExpected) -> addResult (Left err) @?= Left editExpected
            forM_ additionRows $ \(err, expected) -> addResult (Right (Left err)) @?= Left expected
            addResult (Right (Right ())) @?= Right ()
        , testCase "a grant view passes through unchanged" $
            describeResult (Right (Right ownerView)) @?= Right ownerView
        , testCase "stored articles keep their revisions" $ do
            let stored = articleFixture{storedRevision = 7}
                expected = Revised (storedArticle stored) (Revision 7)
            assertSame (Right [expected]) (readResult (Right (Right [stored])))
            assertSame (Right expected) (editResult (Right (Right stored)))
        ]

openingRows :: [(OpenError, ReadFailure, EditFailure)]
openingRows =
    [ (Open.Missing, ReadDenied, EditDenied)
    , (Open.Busy, ReadFault Busy, EditFault Busy)
    , (Open.InvalidMetadata, ReadFault Corrupt, EditFault Corrupt)
    , (Open.Unavailable, ReadFault Unavailable, EditFault Unavailable)
    , (Open.UnsupportedSchema, ReadFault Unavailable, EditFault Unavailable)
    , (Open.DatabaseFault, ReadFault Unavailable, EditFault Unavailable)
    ]

-- Additions take no article identifier or revision, so absence, revision, and conflict errors become faults.
additionRows :: [(StoreError, EditFailure)]
additionRows =
    [ (StoreDenied, EditDenied)
    , (StoreBusy, EditFault Busy)
    , (StoreCorrupt, EditFault Corrupt)
    , (StoreFault, EditFault Unavailable)
    , (StoreTransactionActive, EditFault Unavailable)
    , (StoreNotFound, EditFault Unavailable)
    , (StoreInvalidRevision, EditFault Unavailable)
    , (StoreConflict, EditFault Unavailable)
    , (StoreRevisionExhausted, EditFault Unavailable)
    ]

-- Reads cannot produce absence, revision, or conflict errors, so those become faults.
operationRows :: [(StoreError, ReadFailure, EditFailure)]
operationRows =
    [ (StoreDenied, ReadDenied, EditDenied)
    , (StoreNotFound, ReadFault Unavailable, EditNotFound)
    , (StoreInvalidRevision, ReadFault Unavailable, EditInvalidRevision)
    , (StoreConflict, ReadFault Unavailable, EditConflict)
    , (StoreRevisionExhausted, ReadFault Unavailable, EditRevisionExhausted)
    , (StoreBusy, ReadFault Busy, EditFault Busy)
    , (StoreCorrupt, ReadFault Corrupt, EditFault Corrupt)
    , (StoreFault, ReadFault Unavailable, EditFault Unavailable)
    , (StoreTransactionActive, ReadFault Unavailable, EditFault Unavailable)
    ]

------------------------------------------------------------------------
-- Runners
------------------------------------------------------------------------

runnerTests :: TestTree
runnerTests =
    testGroup
        "Runners"
        [ testCase "operations act only on the bearer's collection" . withCollections $ \directory root -> do
            otherTitle <- requireRight (mkTitle "Other collection")
            createCollection (collectionFile directory otherCollection) otherCollection (ownerGrant otherCollection otherDigest) [retitled otherTitle 0 articleFixture]
            owner <- bindingFor root goldenToken
            other <- bindingFor root otherToken
            newTitle <- requireRight (mkTitle "Changed in A")

            changeWith owner articleIdentifier newTitle (Revision 0) >>= assertSame (Right (revised (retitled newTitle 1 articleFixture)))
            readWith owner AllArticles >>= assertSame (Right [revised (retitled newTitle 1 articleFixture)])
            readWith other (SingleArticle articleIdentifier) >>= assertSame (Right [revised (retitled otherTitle 0 articleFixture)])
        , testCase "an addition through the production runner is listed afterwards" . withCollections $ \_ root -> do
            owner <- bindingFor root goldenToken
            url <- requireRight (parseWebUrl "https://example.com/added")
            title <- requireRight (mkTitle "Added through the runner")
            runEff (runArticleWriter owner (addArticle url title)) >>= (@?= Right ())
            listed <- requireRight =<< readWith owner AllArticles
            map (articleUrl . revisedValue) listed `assertContainsUrl` url
        , testCase "the bound grant is described through the production runner" . withCollections $ \_ root -> do
            owner <- bindingFor root goldenToken
            runEff (runGrantReader owner describeGrant) >>= (@?= Right ownerView)
            bindingCollection owner @?= collection
        , testCase "a missing collection is denied without creating a file" . withCollections $ \directory root -> do
            other <- bindingFor root otherToken
            before <- listDirectory directory

            runEff (runGrantReader other describeGrant) >>= (@?= Left ReadDenied)
            readWith other AllArticles >>= assertLeft ReadDenied
            changeWith other articleIdentifier (articleTitle (storedArticle articleFixture)) (Revision 0) >>= assertLeft EditDenied
            listDirectory directory >>= (@?= before)
        , testCase "a symlinked collection file is a fault, not a denial" . withSystemTempDirectory "eselsohr-runtime" $ \directory -> do
            root <- requireRight =<< mkCollectionDirectory directory
            let target = directory </> "target.sqlite"
            createCollection target collection (ownerGrant collection goldenDigest) [articleFixture]
            createSymbolicLink target (collectionFile directory collection)
            owner <- bindingFor root goldenToken

            readWith owner AllArticles >>= assertLeft (ReadFault Unavailable)
        , testCase "a bearer without ChangeTitles is denied and nothing changes" . withCollections $ \directory root -> do
            addDelegatedReader (collectionFile directory collection)
            reader <- bindingFor root zeroToken
            owner <- bindingFor root goldenToken
            newTitle <- requireRight (mkTitle "Not allowed")

            changeWith reader articleIdentifier newTitle (Revision 0) >>= assertLeft EditDenied
            readWith owner AllArticles >>= assertSame (Right [revised articleFixture])
        , testCase "revoking an ancestor denies the next call" . withCollections $ \directory root -> do
            let path = collectionFile directory collection
            addDelegatedReader path
            reader <- bindingFor root zeroToken

            readWith reader AllArticles >>= assertSame (Right [revised articleFixture])
            requireRight =<< withExistingFile path revokeChild
            readWith reader AllArticles >>= assertLeft ReadDenied
        , testCase "expiry between calls through an advancing clock denies the later call" . withCollections $ \directory root -> do
            addDelegatedReader (collectionFile directory collection)
            now <- newIORef instant
            bearer <- requireRight (parseBearer zeroToken)
            let reader = Binding root bearer (readIORef now)

            readWith reader AllArticles >>= assertSame (Right [revised articleFixture])
            writeIORef now afterDeadline
            readWith reader AllArticles >>= assertLeft ReadDenied
        , testCase "a malformed stored row is corruption" . withCollections $ \directory root -> do
            requireRight =<< withExistingFile (collectionFile directory collection) (`SQL.execute_` "UPDATE article SET url = 'invalid'")
            owner <- bindingFor root goldenToken

            readWith owner AllArticles >>= assertLeft (ReadFault Corrupt)
        , testCase "a held write lock is a retryable fault" . withCollections $ \directory root -> do
            owner <- bindingFor root goldenToken
            newTitle <- requireRight (mkTitle "Blocked")

            SQL.withConnection (collectionFile directory collection) $ \holder -> do
                SQL.execute_ holder "BEGIN IMMEDIATE"
                result <- await (changeWith owner articleIdentifier newTitle (Revision 0))
                assertLeft (EditFault Busy) result
                SQL.execute_ holder "ROLLBACK"
            readWith owner AllArticles >>= assertSame (Right [revised articleFixture])
        , testCase "each operation opens the current collection file" . withCollections $ \directory root -> do
            owner <- bindingFor root goldenToken
            replacementTitle <- requireRight (mkTitle "Replacement")
            let replacement = directory </> "replacement.tmp"

            readWith owner AllArticles >>= assertSame (Right [revised articleFixture])
            createCollection replacement collection (ownerGrant collection goldenDigest) [retitled replacementTitle 3 articleFixture]
            renameFile replacement (collectionFile directory collection)
            readWith owner AllArticles >>= assertSame (Right [revised (retitled replacementTitle 3 articleFixture)])
        , testCase "the bound clock is sampled inside each transaction" . withCollections $ \directory root -> do
            let path = collectionFile directory collection
            newTitle <- requireRight (mkTitle "Sampled")
            bearer <- requireRight (parseBearer goldenToken)

            samples <- newIORef []
            SQL.withConnection path $ \probe -> do
                let owner = Binding root bearer (sampleInsideTransaction probe samples)
                readWith owner AllArticles >>= assertSame (Right [revised articleFixture])
                changeWith owner articleIdentifier newTitle (Revision 0) >>= assertSame (Right (revised (retitled newTitle 1 articleFixture)))

            observed <- readIORef samples
            length observed @?= 2
            assertBool "a clock sample occurred outside a transaction" (and observed)
        ]

------------------------------------------------------------------------
-- Presented bearers
------------------------------------------------------------------------

presentedTests :: TestTree
presentedTests =
    testGroup
        "Presented bearers"
        [ testCase "a created owner bearer binds and reads its collection" . withSystemTempDirectory "eselsohr-bind" $ \path -> do
            directory <- resolveDirectory path >>= maybe (assertFailure "the directory did not resolve") pure
            owner <- requireRight =<< Create.createCollection directory
            case bindPresented directory (renderBearer owner) of
                Nothing -> assertFailure "the owner bearer did not bind"
                Just binding -> runEff (runArticleReader binding (listArticles AllArticles)) >>= assertSame (Right [])
        , testCase "malformed and oversized text does not bind" . withSystemTempDirectory "eselsohr-bind" $ \path -> do
            directory <- resolveDirectory path >>= maybe (assertFailure "the directory did not resolve") pure
            forM_ ["", "v2.", Text.dropEnd 1 goldenToken, goldenToken <> "A", Text.replicate 1000000 "A"] $ \presented ->
                assertBool (show (Text.take 80 presented)) (isNothing (bindPresented directory presented))
            listDirectory path >>= (@?= [])
        , testCase "a missing path or regular file does not resolve" . withSystemTempDirectory "eselsohr-bind" $ \path -> do
            writeFile (path </> "file") ""
            forM_ [path </> "missing", path </> "file"] $ \candidate ->
                resolveDirectory candidate >>= assertBool candidate . isNothing
        ]

------------------------------------------------------------------------
-- Application functions
------------------------------------------------------------------------

applicationTests :: TestTree
applicationTests =
    testGroup
        "Application functions"
        [ testCase "a read and title change at the returned revision runs through the production runners" . withCollections $ \_ root -> do
            owner <- bindingFor root goldenToken
            newTitle <- requireRight (mkTitle "Retitled")
            let expected = revised (retitled newTitle 1 articleFixture)

            runEff (runArticleWriter owner . runArticleReader owner $ retitleArticle articleIdentifier newTitle) >>= assertSame (Right expected)
            readWith owner AllArticles >>= assertSame (Right [expected])
        , testCase "a missing article stops before the title change" . withCollections $ \_ root -> do
            owner <- bindingFor root goldenToken
            missing <- requireRight (parseArticleId "123e4567-e89b-42d3-a456-426614174099")

            runEff (runArticleWriter owner . runArticleReader owner $ retitleArticle missing (articleTitle (storedArticle articleFixture))) >>= assertLeft NoSingleArticle
        , testCase "a read-only route runs with only the reader under an owner bearer" . withCollections $ \_ root -> do
            owner <- bindingFor root goldenToken
            runEff (runArticleReader owner readOnlyRoute) >>= assertSame (Right [articleTitle (storedArticle articleFixture)])
        ]

-- | A route composed with only the reader: its effects admit no write, whatever the bearer permits.
readOnlyRoute :: Eff '[ArticleReader, IOE] (Either ReadFailure [Title])
readOnlyRoute = fmap (map (articleTitle . revisedValue)) <$> listArticles AllArticles

data RetitleFailure = ReadFailed ReadFailure | NoSingleArticle | EditFailed EditFailure
    deriving stock (Eq, Show)

-- | Read one article and change its title at the revision just read.
retitleArticle :: (ArticleReader :> es, ArticleWriter :> es) => ArticleId -> Title -> Eff es (Either RetitleFailure (Revised Article))
retitleArticle identifier title = do
    selected <- listArticles (SingleArticle identifier)
    case selected of
        Left failure -> pure (Left (ReadFailed failure))
        Right [Revised _ current] -> first EditFailed <$> changeTitle identifier title current
        Right _ -> pure (Left NoSingleArticle)

------------------------------------------------------------------------
-- Cleanup
------------------------------------------------------------------------

cleanupTests :: TestTree
cleanupTests =
    testGroup
        "Cleanup"
        [ testCase "an exception from the bound clock propagates after rollback" . withCollections $ \directory root -> do
            bearer <- requireRight (parseBearer goldenToken)
            newTitle <- requireRight (mkTitle "Never stored")
            let failing = Binding root bearer (throwIO CallbackFailure)

            result <- try @CallbackFailure (changeWith failing articleIdentifier newTitle (Revision 0))
            case result of
                Left exception -> exception @?= CallbackFailure
                Right _ -> assertFailure "exception was swallowed"
            assertUnchangedAndReleased directory root
        , testCase "cancellation inside a write transaction propagates after rollback" . withCollections $ \directory root -> do
            bearer <- requireRight (parseBearer goldenToken)
            newTitle <- requireRight (mkTitle "Never stored")
            entered <- newEmptyMVar
            blocked <- newEmptyMVar @()
            finished <- newEmptyMVar
            let blocking = Binding root bearer (putMVar entered () >> takeMVar blocked >> pure instant)
                action = changeWith blocking articleIdentifier newTitle (Revision 0)

            bracket (forkFinally action (putMVar finished)) killThread $ \thread -> do
                await (takeMVar entered)
                killThread thread

                outcome <- await (takeMVar finished)
                case outcome of
                    Left exception -> fromException exception @?= Just ThreadKilled
                    Right _ -> assertFailure "cancellation was swallowed"
            assertUnchangedAndReleased directory root
        ]

-- | The article keeps its title and revision, and another connection can take the write lock.
assertUnchangedAndReleased :: FilePath -> CollectionDirectory -> Assertion
assertUnchangedAndReleased directory root = do
    owner <- bindingFor root goldenToken
    readWith owner AllArticles >>= assertSame (Right [revised articleFixture])
    released <- withExistingFile (collectionFile directory collection) $ \other ->
        bracket_ (SQL.execute_ other "BEGIN EXCLUSIVE") (SQL.execute_ other "ROLLBACK") (pure ())
    requireRight released

------------------------------------------------------------------------
-- Runner fixtures
------------------------------------------------------------------------

readWith :: Binding -> ArticleSelection -> IO (Either ReadFailure [Revised Article])
readWith binding = runEff . runArticleReader binding . listArticles

changeWith :: Binding -> ArticleId -> Title -> Revision -> IO (Either EditFailure (Revised Article))
changeWith binding identifier title = runEff . runArticleWriter binding . changeTitle identifier title

bindingFor :: CollectionDirectory -> Text.Text -> IO Binding
bindingFor root token = do
    bearer <- requireRight (parseBearer token)
    pure (Binding root bearer (pure instant))

-- | A collections directory containing collection A with its owner grant and one article.
withCollections :: (FilePath -> CollectionDirectory -> IO a) -> IO a
withCollections use = withSystemTempDirectory "eselsohr-runtime" $ \directory -> do
    root <- requireRight =<< mkCollectionDirectory directory
    createCollection (collectionFile directory collection) collection (ownerGrant collection goldenDigest) [articleFixture]
    use directory root

createCollection :: FilePath -> CollectionId -> StoredGrant -> [StoredArticle] -> IO ()
createCollection path owner grant articles = do
    writeFile path ""
    created <- withExistingFile path $ \connection -> do
        requireRight =<< initializeSchema connection owner
        insertGrant connection owner grant
        mapM_ (insertArticle connection) articles
    requireRight created

collectionFile :: FilePath -> CollectionId -> FilePath
collectionFile directory owner = directory </> Text.unpack (renderCollectionId owner) <> ".sqlite"

ownerGrant :: CollectionId -> ByteString.ByteString -> StoredGrant
ownerGrant owner digest = rootFixture{storedGrant = (storedGrant rootFixture){grantScope = WholeCollection owner}, storedTokenHash = digest}

-- | Delegate read-only access from the owner through a child to the grandchild bearer for 'zeroToken'.
addDelegatedReader :: FilePath -> IO ()
addDelegatedReader path = do
    grandchild <- requireRight (parseCapabilityId "123e4567-e89b-42d3-a456-426614174005")
    let delegated identifier parent permissions depth digest = StoredGrant (GrantSnapshot identifier (Just parent) (WholeCollection collection) (Set.fromList permissions) (Just deadline) depth) digest Nothing
    inserted <- withExistingFile path $ \connection -> do
        insertGrant connection collection (delegated childIdentifier rootIdentifier [ReadArticles, Delegate] 1 (ByteString.replicate 32 2))
        insertGrant connection collection (delegated grandchild childIdentifier [ReadArticles] 2 zeroDigest)
    requireRight inserted

revokeChild :: Connection -> IO ()
revokeChild connection = SQL.execute connection "DELETE FROM capability WHERE id = ?" . Only =<< requireRight (toCapabilityBytes childIdentifier)

-- | Record whether another connection is locked out, which holds while a transaction has read the collection.
sampleInsideTransaction :: Connection -> IORef [Bool] -> IO UTCTime
sampleInsideTransaction probe samples = do
    attempt <- try @SQLError (SQL.execute_ probe "BEGIN EXCLUSIVE")
    locked <- case attempt of
        Left err -> pure (sqlError err == SQLite.ErrorBusy)
        Right () -> False <$ SQL.execute_ probe "ROLLBACK"
    modifyIORef' samples (locked :)
    pure instant

retitled :: Title -> Integer -> StoredArticle -> StoredArticle
retitled title number stored = StoredArticle (rename title (storedArticle stored)) number

assertContainsUrl :: [WebUrl] -> WebUrl -> Assertion
assertContainsUrl urls url = assertBool "the added URL is not listed" (url `elem` urls)

ownerView :: GrantView
ownerView = GrantView (Set.fromList [minBound .. maxBound]) (WholeCollection collection) Nothing

instant, deadline, afterDeadline :: UTCTime
instant = fromTimestamp 1000000
deadline = fromTimestamp 2000000
afterDeadline = fromTimestamp 2000001
