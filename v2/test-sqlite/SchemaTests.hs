{- |
Module      : SchemaTests
Description : Schema installation and constraint tests

'schemaTests' checks initial installation, collection identity, schema versions, and database constraints.
Transaction cases cover existing transactions, contention, failed initialization, rollback, cancellation, and the caller's exception masking state.
-}
module SchemaTests (schemaTests) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), MaskingState (..), bracket, fromException, getMaskingState, mask_, throwIO, try)
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Open
import Eselsohr.Store.SQLite.Schema
import SqliteFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------

schemaTests :: TestTree
schemaTests =
    testGroup
        "Schema"
        [ testCase "initialize once, preserve the identity, and create no grants" . withEmptyDatabase $ \_ root connection -> do
            initializeSchema connection collection >>= (@?= Right ())
            checkSchemaVersion connection >>= (@?= Right ())
            SQL.query_ connection "SELECT name FROM sqlite_schema WHERE type = 'table' ORDER BY name" >>= (@?= map Only (["article", "capability", "capability_permission", "collection_meta"] :: [Text]))
            SQL.query_ connection "SELECT collection_id FROM collection_meta" >>= (@?= [Only collectionBytes])
            SQL.query_ connection "SELECT count(*) FROM capability" >>= (@?= [Only (0 :: Int)])

            initializeSchema connection otherCollection >>= (@?= Left NonEmptyDatabase)
            withExistingCollection root collection (\_ -> pure ()) >>= (@?= Right ())
        , testCase "an existing object is preserved" . withEmptyDatabase $ \_ _ connection -> do
            SQL.execute_ connection "CREATE TABLE sqliteXexisting (value TEXT)"
            SQL.execute_ connection "INSERT INTO sqliteXexisting VALUES ('keep')"

            initializeSchema connection collection >>= (@?= Left NonEmptyDatabase)
            SQL.query_ connection "SELECT value FROM sqliteXexisting" >>= (@?= [Only ("keep" :: Text)])
            assertAutocommit connection
        , testCase "nonzero versions reject initialization of an otherwise empty database" . withEmptyDatabase $ \_ _ connection -> do
            forM_ [-1, 1, 2, 2147483647 :: Int64] $ \version -> do
                setVersion connection version
                initializeSchema connection collection >>= (@?= Left NonEmptyDatabase)
                SQL.query_ connection "PRAGMA user_version" >>= (@?= [Only version])
        , testCase "unsupported collection versions never reach the callback or change the file" . withEmptyDatabase $ \path root connection -> do
            initializeSchema connection collection >>= (@?= Right ())
            forM_ [0, -1, 2, 2147483647] $ \version -> do
                setVersion connection version
                before <- ByteString.readFile path

                withExistingCollection root collection unexpectedCallback >>= (@?= Left UnsupportedSchema)
                ByteString.readFile path >>= (@?= before)

            setVersion connection 1
            withExistingCollection root collection (\_ -> pure ()) >>= (@?= Right ())
        , testCase "an active transaction is not committed or rolled back" . withEmptyDatabase $ \_ _ connection -> do
            SQL.execute_ connection "BEGIN"
            SQL.execute_ connection "CREATE TABLE existing (value TEXT)"

            initializeSchema connection collection >>= (@?= Left TransactionActive)
            Direct.getAutoCommit (connectionHandle connection) >>= (@?= False)

            SQL.execute_ connection "ROLLBACK"
            assertEmpty connection
        , testCase "disabled foreign keys or journaling reject initialization" . withEmptyDatabase $ \_ _ connection -> do
            SQL.execute_ connection "PRAGMA foreign_keys = OFF"
            initializeSchema connection collection >>= (@?= Left SchemaFault)
            SQL.execute_ connection "PRAGMA foreign_keys = ON"
            SQL.query_ connection "PRAGMA journal_mode = OFF" >>= (@?= [Only ("off" :: Text)])
            initializeSchema connection collection >>= (@?= Left SchemaFault)
            assertEmpty connection
        , testCase "database-full failure leaves no partial schema or version" . withEmptyDatabase $ \_ _ connection -> do
            SQL.query_ connection "PRAGMA max_page_count = 3" >>= (@?= [Only (3 :: Int)])
            initializeSchema connection collection >>= (@?= Left SchemaFault)
            assertEmpty connection
            SQL.query_ connection "PRAGMA max_page_count = 1000" >>= (@?= [Only (1000 :: Int)])
            initializeSchema connection collection >>= (@?= Right ())
        , testCase "another writer blocks BEGIN without holding new locks" . withEmptyDatabase $ \path _ connection ->
            SQL.withConnection path $ \holder -> do
                SQL.execute_ holder "BEGIN IMMEDIATE"
                await (initializeSchema connection collection) >>= (@?= Left SchemaBusy)
                assertAutocommit connection

                SQL.execute_ holder "ROLLBACK"
                initializeSchema connection collection >>= (@?= Right ())
        , testCase "a reader blocking COMMIT causes rollback and releases the writer" . withEmptyDatabase $ \path _ connection -> do
            SQL.execute_ connection "CREATE TABLE scratch (value INTEGER)"
            SQL.execute_ connection "DROP TABLE scratch"

            SQL.withConnection path $ \reader -> do
                SQL.execute_ reader "BEGIN"
                SQL.query_ reader "SELECT count(*) FROM sqlite_schema" >>= (@?= [Only (0 :: Int)])
                await (initializeSchema connection collection) >>= (@?= Left SchemaBusy)
                assertEmpty connection
                SQL.execute_ reader "ROLLBACK"

            initializeSchema connection collection >>= (@?= Right ())
        , testCase "transaction body preserves caller masking state" . withEmptyDatabase $ \_ _ connection -> do
            let check expected = do
                    result <- withSchemaTransaction connection (Right <$> getMaskingState)
                    result @?= Right expected
                    assertAutocommit connection

            check Unmasked
            mask_ (check MaskedInterruptible)
        , testCase "controlled failure rolls back the body" . withEmptyDatabase $ \_ _ connection -> do
            withSchemaTransaction connection (partialSchema connection >> pure (Left NonEmptyDatabase :: Either SchemaError ())) >>= (@?= Left NonEmptyDatabase)
            assertEmpty connection
        , testCase "SQL failure rolls back the body" . withEmptyDatabase $ \_ _ connection -> do
            result <- withSchemaTransaction connection $ do
                partialSchema connection
                SQL.execute_ connection "INSERT INTO missing VALUES (1)"
                pure (Right ())

            result @?= Left SchemaFault
            assertEmpty connection
        , testCase "unexpected exception is preserved after rollback" . withEmptyDatabase $ \_ _ connection -> do
            result <- try @CallbackFailure . withSchemaTransaction connection $ do
                partialSchema connection
                throwIO CallbackFailure

            result @?= (Left CallbackFailure :: Either CallbackFailure (Either SchemaError ()))
            assertEmpty connection
        , testCase "cancellation before commit rolls back the body" . withEmptyDatabase $ \_ _ connection -> do
            entered <- newEmptyMVar
            blocked <- newEmptyMVar @()
            finished <- newEmptyMVar
            let action = withSchemaTransaction connection $ do
                    partialSchema connection
                    putMVar entered ()
                    takeMVar blocked
                    pure (Right ())

            bracket (forkFinally action (putMVar finished)) killThread $ \thread -> do
                await (takeMVar entered)
                killThread thread

                outcome <- await (takeMVar finished)
                case outcome of
                    Left err -> fromException err @?= Just ThreadKilled
                    Right _ -> assertFailure "cancellation was swallowed"

            assertEmpty connection
        , constraintTests
        ]

------------------------------------------------------------------------
-- Constraints
------------------------------------------------------------------------

constraintTests :: TestTree
constraintTests =
    testGroup
        "Constraints"
        [ testCase "revision defaults to zero and foreign key check is clean" . withDatabase $ \connection -> do
            row <- requireRight (toArticleRow articleFixture)
            SQL.execute connection "INSERT INTO article (id, title, url, state, created_at) VALUES (?, ?, ?, ?, ?)" (rowArticleId row, rowTitle row, rowUrl row, rowState row, rowCreatedAt row)
            SQL.query_ connection "SELECT revision FROM article" >>= (@?= [Only (0 :: Int)])
            insertGrant connection collection rootFixture
            insertGrant connection collection childFixture
            SQL.query_ connection "PRAGMA foreign_key_check" >>= (@?= ([] :: [[SQL.SQLData]]))
        , testCase "article keys, lengths, enums, nulls, and revision bounds" . withDatabase $ \connection -> do
            insertArticle connection articleFixture
            assertConstraint (insertArticle connection articleFixture)
            forM_
                [ "UPDATE article SET id = x'00'"
                , "UPDATE article SET id = NULL"
                , "UPDATE article SET title = ''"
                , "UPDATE article SET url = ''"
                , "UPDATE article SET state = 'other'"
                , "UPDATE article SET created_at = NULL"
                , "UPDATE article SET revision = -1"
                , "UPDATE article SET revision = 9223372036854775808"
                ]
                $ assertConstraint . SQL.execute_ connection
        , testCase "grant keys, digests, roots, deadlines, depths, and parent relationships" . withDatabase $ \connection -> do
            insertArticle connection articleFixture
            insertGrant connection collection rootFixture
            assertConstraint (insertGrant connection collection rootFixture)
            assertConstraint (SQL.execute_ connection "INSERT INTO capability SELECT randomblob(16), randomblob(32), NULL, NULL, NULL, 0, NULL FROM capability")
            insertGrant connection collection childFixture
            forM_
                [ "UPDATE capability SET id = x'00' WHERE parent_id IS NOT NULL"
                , "UPDATE capability SET token_hash = x'00'"
                , "UPDATE capability SET token_hash = NULL"
                , "UPDATE capability SET token_hash = (SELECT token_hash FROM capability WHERE parent_id IS NULL) WHERE parent_id IS NOT NULL"
                , "UPDATE capability SET parent_id = id WHERE parent_id IS NOT NULL"
                , "UPDATE capability SET parent_id = randomblob(16) WHERE parent_id IS NOT NULL"
                , "UPDATE capability SET scope_article_id = randomblob(16) WHERE parent_id IS NOT NULL"
                , "UPDATE capability SET expires_at = 0 WHERE parent_id IS NULL"
                , "UPDATE capability SET scope_article_id = (SELECT id FROM article) WHERE parent_id IS NULL"
                , "UPDATE capability SET depth = 1 WHERE parent_id IS NULL"
                , "UPDATE capability SET expires_at = NULL WHERE parent_id IS NOT NULL"
                , "UPDATE capability SET depth = 0 WHERE parent_id IS NOT NULL"
                , "UPDATE capability SET depth = 5 WHERE parent_id IS NOT NULL"
                ]
                $ assertConstraint . SQL.execute_ connection
        , testCase "permission rows reject duplicates, unknown names, and missing grants" . withDatabase $ \connection -> do
            insertGrant connection collection rootFixture
            forM_
                [ "INSERT INTO capability_permission SELECT capability_id, permission FROM capability_permission LIMIT 1"
                , "INSERT INTO capability_permission SELECT id, 'unknown' FROM capability"
                , "INSERT INTO capability_permission VALUES (randomblob(16), 'read')"
                ]
                $ assertConstraint . SQL.execute_ connection
        , testCase "article deletion cascades through scoped descendants and permissions" . withDatabase $ \connection -> do
            insertArticle connection articleFixture
            insertGrant connection collection rootFixture
            insertGrant connection collection childFixture
            SQL.execute_ connection "INSERT INTO capability SELECT randomblob(16), randomblob(32), id, scope_article_id, expires_at, 2, NULL FROM capability WHERE parent_id IS NOT NULL"
            SQL.execute_ connection "INSERT INTO capability_permission SELECT id, 'read' FROM capability WHERE depth = 2"

            SQL.execute_ connection "DELETE FROM article"
            SQL.query_ connection "SELECT depth FROM capability" >>= (@?= [Only (0 :: Int)])
            SQL.query_ connection "SELECT count(*) FROM capability_permission" >>= (@?= [Only (8 :: Int)])

            SQL.execute_ connection "DELETE FROM capability"
            SQL.query_ connection "SELECT count(*) FROM capability_permission" >>= (@?= [Only (0 :: Int)])
        ]

------------------------------------------------------------------------
-- Fixtures and database checks
------------------------------------------------------------------------

assertConstraint :: IO () -> Assertion
assertConstraint action = do
    result <- try @SQL.SQLError action
    case result of
        Left err -> SQL.sqlError err @?= SQLite.ErrorConstraint
        Right () -> assertFailure "constraint accepted invalid input"

assertAutocommit :: Connection -> Assertion
assertAutocommit connection = Direct.getAutoCommit (connectionHandle connection) >>= (@?= True)

assertEmpty :: Connection -> Assertion
assertEmpty connection = do
    assertAutocommit connection
    SQL.query_ connection "PRAGMA user_version" >>= (@?= [Only (0 :: Int)])
    SQL.query_ connection "SELECT count(*) FROM sqlite_schema WHERE name NOT GLOB 'sqlite_*'" >>= (@?= [Only (0 :: Int)])

partialSchema :: Connection -> IO ()
partialSchema connection = do
    SQL.execute_ connection "CREATE TABLE partial (value INTEGER)"
    SQL.execute_ connection "PRAGMA user_version = 7"

setVersion :: Connection -> Int64 -> IO ()
setVersion connection version = SQL.execute_ connection . SQL.Query $ "PRAGMA user_version = " <> Text.pack (show version)

unexpectedCallback :: Connection -> IO ()
unexpectedCallback _ = assertFailure "callback ran"
