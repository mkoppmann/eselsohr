{- |
Module      : Main
Description : SQLite adapter test suite entry point and opening tests

The SQLite suite reports the engine version and compile options, verifies the expected bundled engine, and runs all adapter test groups.
This module also contains the collection opening, metadata validation, and connection lifetime tests, including callback failures and cancellation.
-}
module Main (main) where

import BearerTests (bearerTests)
import CodecTests (codecTests)
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), Exception, bracket, fromException, throwIO, try)
import Control.Monad (forM_, (>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (readIORef)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding.Error (UnicodeException (..))
import Database.SQLite.Simple (Connection (..), Only (..), SQLData (..), SQLError (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Eselsohr.Id (CollectionId, parseCollectionId, renderCollectionId)
import Eselsohr.Store.SQLite.Open
import Eselsohr.Store.SQLite.Schema qualified as Schema
import ReadTests (readTests)
import SchemaTests (schemaTests)
import System.Directory (createDirectory, doesPathExist, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, createSymbolicLink, ownerModes)
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TitleTests (titleTests)

main :: IO ()
main = do
    withFixture $ \_ root -> do
        report <- withExistingCollection root collection $ \connection -> do
            version <- SQL.query_ connection "SELECT sqlite_version(), sqlite_source_id()" :: IO [(Text, Text)]
            options <- SQL.query_ connection "PRAGMA compile_options" :: IO [Only Text]

            putStrLn ("SQLite version and source: " <> show version)
            putStrLn ("SQLite compile options: " <> show options)
            map fst version @?= ["3.45.0"]
        report @?= Right ()

    defaultMain . localOption (mkTimeout 5000000) $ testGroup "SQLite" [openingTests, metadataTests, lifetimeTests, schemaTests, codecTests, bearerTests, readTests, titleTests]

------------------------------------------------------------------------
-- Restricted opening
------------------------------------------------------------------------

openingTests :: TestTree
openingTests =
    testGroup
        "Restricted opening"
        [ testCase "identity, settings, binding, and nested savepoints" . withFixture $ \_ root -> do
            result <- withExistingCollection root collection $ \connection -> do
                readIORef (connectionTempNameCounter connection) >>= (@?= 0)
                mapM (SQL.query_ connection) ["PRAGMA foreign_keys", "PRAGMA trusted_schema", "PRAGMA busy_timeout"]
                    >>= (@?= [[Only (1 :: Int)], [Only 0], [Only 0]])

                let title = "quotes ' \" ; -- ? & # é 😀" :: Text
                SQL.withSavepoint connection . SQL.withSavepoint connection $ do
                    SQL.execute connection "UPDATE probe SET title = ? WHERE id = ?" (title, "same-id" :: Text)
                    SQL.query connection "SELECT title FROM probe WHERE id = ?" (Only ("same-id" :: Text)) >>= (@?= [Only title])
                readIORef (connectionTempNameCounter connection) >>= (@?= 2)
            result @?= Right ()
        , testCase "missing directory is not created" . withSystemTempDirectory "eselsohr" $ \parent -> do
            let missing = parent </> "missing"
            result <- mkCollectionDirectory missing
            assertUnavailable result
            doesPathExist missing >>= (@?= False)
        , testCase "directory constructor rejects files and invalid strings" . withFixture $ \path _ -> do
            forM_ [path, "", path <> "\0suffix", path <> "\xD800"] (mkCollectionDirectory >=> assertUnavailable)
        , testCase "directory alias is canonicalized once" . withRoot $ \directory _ -> do
            let alias = directory </> "alias"
            createSymbolicLink directory alias
            root <- requireRight =<< mkCollectionDirectory (alias </> ".")
            removeFile alias
            createFixture (collectionPath directory collection)

            withExistingCollection root collection readTitle >>= (@?= Right "original")
        , testCase "missing file stays missing at both layers" . withRoot $ \directory root -> do
            let path = collectionPath directory collection
            assertRejected Unavailable (withExistingCollection root collection) directory []
            assertRejected Unavailable (withExistingFile path) directory []
        , testCase "absolute paths preserve spaces, Unicode, and URI characters" . withSystemTempDirectory "eselsohr" $ \parent -> do
            let directory = parent </> "file:collections é ?mode=ro&cache=shared#%20"
            createDirectory directory
            root <- requireRight =<< mkCollectionDirectory directory
            createFixture (collectionPath directory collection)
            withExistingCollection root collection readTitle >>= (@?= Right "original")
        , testCase "raw opening rejects relative, NUL, and surrogate paths before opening a valid prefix" . withFixture $ \path _ -> do
            forM_ ["file:relative?mode=rw", path <> "\0suffix", path <> "\xDFFF"] $ \input -> do
                result <- withExistingFile input unexpectedCallback
                result @?= Left Unavailable
        , testGroup "symbolic links" [linkTest False, linkTest True]
        , testCase "directory targets are rejected" . withRoot $ \directory root -> do
            createDirectory (collectionPath directory collection)
            withExistingCollection root collection unexpectedCallback >>= (@?= Left Unavailable)
        , testCase "FIFO targets are rejected without opening" . withRoot $ \directory root -> do
            createNamedPipe (collectionPath directory collection) ownerModes
            await (withExistingCollection root collection unexpectedCallback) >>= (@?= Left Unavailable)
        , testCase "empty file is not initialized" $ rejectedFile ByteString.empty
        , testCase "non-SQLite file is unchanged" $ rejectedFile "not a SQLite database"
        , testCase "collection identities separate repeated object IDs" . withRoot $ \directory root -> do
            createFixture (collectionPath directory collection)
            let secondPath = collectionPath directory otherCollection
            SQL.withConnection secondPath $ \connection -> do
                initializeFixture connection otherCollection
                SQL.execute_ connection "INSERT INTO probe VALUES ('same-id', 'second')"

            withExistingCollection root collection readTitle >>= (@?= Right "original")
            withExistingCollection root otherCollection readTitle >>= (@?= Right "second")

            withExistingCollection root collection (`SQL.execute_` "UPDATE probe SET title = 'changed'") >>= (@?= Right ())
            withExistingCollection root otherCollection readTitle >>= (@?= Right "second")
        , testCase "exclusive lock fails fast as Busy and releases cleanly" . withFixture $ \path root ->
            SQL.withConnection path $ \holder -> do
                SQL.execute_ holder "BEGIN EXCLUSIVE"
                await (withExistingCollection root collection unexpectedCallback) >>= (@?= Left Busy)
                SQL.execute_ holder "ROLLBACK"
                assertReleased root
        ]

linkTest :: Bool -> TestTree
linkTest dangling = testCase (if dangling then "dangling link at both layers" else "link to valid file at both layers") $
    withRoot $ \directory root -> do
        let target = directory </> "target.sqlite"
            path = collectionPath directory collection
        if dangling then pure () else createFixture target
        createSymbolicLink target path

        let files = [target | not dangling]
        assertRejected Unavailable (withExistingCollection root collection) directory files
        assertRejected Unavailable (withExistingFile path) directory files

------------------------------------------------------------------------
-- Collection metadata
------------------------------------------------------------------------

metadataTests :: TestTree
metadataTests =
    testGroup "Collection metadata" $
        [ malformed "missing table" (\_ -> pure ())
        , malformed "view is not an identity table" $ \connection -> do
            SQL.execute_ connection "CREATE TABLE actual_meta (singleton INTEGER, collection_id BLOB)"
            SQL.execute connection "INSERT INTO actual_meta VALUES (1, ?)" (Only collectionBytes)
            SQL.execute_ connection "CREATE VIEW collection_meta AS SELECT * FROM actual_meta"
        , malformed "missing columns" $ \connection -> SQL.execute_ connection "CREATE TABLE collection_meta (unrelated TEXT)"
        , malformed "no identity row" relaxedSchema
        , malformed "duplicate identity rows" $ \connection -> do
            relaxedSchema connection
            insertIdentity connection collectionBytes
            insertIdentity connection collectionBytes
        , malformed "mismatched collection" $ \connection -> makeSchema connection >> insertIdentity connection otherBytes
        , testCase "STRICT metadata schema enforces its constraints" . withFixture $ \path _ -> SQL.withConnection path $ \connection -> do
            assertSqlError (SQL.execute connection "INSERT INTO collection_meta VALUES (1, ?)" (Only collectionBytes))
            SQL.execute_ connection "DELETE FROM collection_meta"
            forM_
                [ [SQLInteger 2, SQLBlob collectionBytes]
                , [SQLInteger 1, SQLBlob "short"]
                , [SQLInteger 1, SQLText "123e4567-e89b-42d3-a456-426614174000"]
                , [SQLInteger 1, SQLNull]
                ]
                $ \row -> assertSqlError (SQL.execute connection "INSERT INTO collection_meta VALUES (?, ?)" row)
            insertIdentity connection collectionBytes
        , testCase "invalid metadata closes the connection before repair" . withRoot $ \directory root -> do
            let path = collectionPath directory collection
            SQL.withConnection path $ \connection -> makeSchema connection >> insertIdentity connection otherBytes
            withExistingCollection root collection unexpectedCallback >>= (@?= Left InvalidMetadata)

            SQL.withConnection path $ \connection -> SQL.withExclusiveTransaction connection $ do
                SQL.execute_ connection "DELETE FROM collection_meta"
                insertIdentity connection collectionBytes
            withExistingCollection root collection (\_ -> pure ()) >>= (@?= Right ())
        ]
            ++ [ testCase ("invalid UTF-8 " <> name <> " rejects metadata and releases locks") . withRoot $ \directory root -> do
                    let path = collectionPath directory collection
                    SQL.withConnection path $ \connection -> do
                        relaxedSchema connection
                        SQL.execute_ connection "PRAGMA user_version = 1"
                        SQL.execute connection insert (Only collectionBytes)

                    assertRejected InvalidMetadata (withExistingCollection root collection) directory [path]
                    SQL.withConnection path $ \connection -> SQL.withExclusiveTransaction connection $ do
                        SQL.execute_ connection "DELETE FROM collection_meta"
                        insertIdentity connection collectionBytes
                    withExistingCollection root collection (\_ -> pure ()) >>= (@?= Right ())
               | (name, insert) <-
                    [ ("singleton", "INSERT INTO collection_meta VALUES (CAST(x'ff' AS TEXT), ?)")
                    , ("identity", "INSERT INTO collection_meta VALUES (1, CAST(? || x'ff' AS TEXT))")
                    ]
               ]
            ++ [ malformed name $ \connection -> do
                    relaxedSchema connection
                    SQL.execute connection "INSERT INTO collection_meta VALUES (?, ?)" row
               | (name, row) <-
                    [ ("wrong singleton", [SQLInteger 2, SQLBlob collectionBytes])
                    , ("text singleton", [SQLText "1", SQLBlob collectionBytes])
                    , ("real singleton", [SQLFloat 1, SQLBlob collectionBytes])
                    , ("null singleton", [SQLNull, SQLBlob collectionBytes])
                    , ("short UUID", [SQLInteger 1, SQLBlob (ByteString.take 15 collectionBytes)])
                    , ("long UUID", [SQLInteger 1, SQLBlob (collectionBytes <> "x")])
                    , ("wrong UUID version", [SQLInteger 1, SQLBlob (replaceByte 6 0x12 collectionBytes)])
                    , ("wrong UUID variant", [SQLInteger 1, SQLBlob (replaceByte 8 0xc4 collectionBytes)])
                    , ("null identity", [SQLInteger 1, SQLNull])
                    , ("text identity", [SQLInteger 1, SQLText (renderCollectionId collection)])
                    , ("integer identity", [SQLInteger 1, SQLInteger 1])
                    , ("real identity", [SQLInteger 1, SQLFloat 1])
                    ]
               ]

------------------------------------------------------------------------
-- Connection lifetime
------------------------------------------------------------------------

lifetimeTests :: TestTree
lifetimeTests =
    testGroup
        "Connection lifetime"
        [ testCase "normal return rolls back unfinished work and releases locks" . withFixture $ \_ root -> do
            withExistingCollection root collection beginChange >>= (@?= Right ())
            assertReleased root
        , testCase "callback exception propagates and releases locks" . withFixture $ \_ root -> do
            result <- try @CallbackFailure . withExistingCollection root collection $ \connection -> do
                beginChange connection
                throwIO CallbackFailure

            result @?= (Left CallbackFailure :: Either CallbackFailure (Either OpenError ()))
            assertReleased root
        , testCase "callback SQLite exception is not translated" . withFixture $ \_ root -> do
            let original = SQLError SQLite.ErrorBusy "callback details" "callback context"
            result <- try @SQLError . withExistingCollection root collection $ \connection -> do
                beginChange connection
                throwIO original

            case result of
                Left exception -> show exception @?= show original
                Right (_ :: Either OpenError ()) -> assertFailure "callback SQLite exception was swallowed"
            assertReleased root
        , testCase "callback conversion exception propagates and releases locks" . withFixture $ \_ root -> do
            let original = DecodeError "callback details" (Just 0xff)
            result <- try @UnicodeException . withExistingCollection root collection $ \connection -> do
                beginChange connection
                throwIO original

            case result of
                Left exception -> show exception @?= show original
                Right (_ :: Either OpenError ()) -> assertFailure "callback conversion exception was swallowed"
            assertReleased root
        , testCase "asynchronous cancellation rolls back and releases locks" . withFixture $ \_ root -> do
            entered <- newEmptyMVar
            blocked <- newEmptyMVar @()
            finished <- newEmptyMVar
            let action = withExistingCollection root collection $ \connection -> do
                    beginChange connection
                    putMVar entered ()
                    takeMVar blocked

            bracket (forkFinally action (putMVar finished)) killThread $ \thread -> do
                await (takeMVar entered)
                killThread thread

                outcome <- await (takeMVar finished)
                case outcome of
                    Left exception -> fromException exception @?= Just ThreadKilled
                    Right _ -> assertFailure "cancellation was swallowed"

            assertReleased root
        ]

------------------------------------------------------------------------
-- Fixtures and database checks
------------------------------------------------------------------------

data CallbackFailure = CallbackFailure deriving stock (Eq, Show)
instance Exception CallbackFailure

collection, otherCollection :: CollectionId
collection = either (error . show) id (parseCollectionId "123e4567-e89b-42d3-a456-426614174000")
otherCollection = either (error . show) id (parseCollectionId "123e4567-e89b-42d3-a456-426614174001")

collectionBytes, otherBytes :: ByteString
collectionBytes = ByteString.pack [0x12, 0x3e, 0x45, 0x67, 0xe8, 0x9b, 0x42, 0xd3, 0xa4, 0x56, 0x42, 0x66, 0x14, 0x17, 0x40, 0x00]
otherBytes = ByteString.take 15 collectionBytes <> ByteString.singleton 1

replaceByte :: Int -> Word -> ByteString -> ByteString
replaceByte offset value bytes = ByteString.take offset bytes <> ByteString.singleton (fromIntegral value) <> ByteString.drop (offset + 1) bytes

collectionPath :: FilePath -> CollectionId -> FilePath
collectionPath directory ident = directory </> Text.unpack (renderCollectionId ident) <> ".sqlite"

withRoot :: (FilePath -> CollectionDirectory -> IO a) -> IO a
withRoot use = withSystemTempDirectory "eselsohr-sqlite" $ \directory -> do
    root <- requireRight =<< mkCollectionDirectory directory
    use directory root

withFixture :: (FilePath -> CollectionDirectory -> IO a) -> IO a
withFixture use = withRoot $ \directory root -> do
    let path = collectionPath directory collection
    createFixture path
    use path root

createFixture :: FilePath -> IO ()
createFixture path = SQL.withConnection path $ \connection -> do
    initializeFixture connection collection
    SQL.execute_ connection "INSERT INTO probe VALUES ('same-id', 'original')"

initializeFixture :: Connection -> CollectionId -> IO ()
initializeFixture connection owner = do
    SQL.execute_ connection "PRAGMA foreign_keys = ON"
    requireRight =<< Schema.initializeSchema connection owner
    SQL.execute_ connection "CREATE TABLE probe (id TEXT PRIMARY KEY, title TEXT NOT NULL) STRICT"

makeSchema :: Connection -> IO ()
makeSchema connection = do
    SQL.execute_ connection "CREATE TABLE collection_meta (singleton INTEGER PRIMARY KEY CHECK (singleton = 1), collection_id BLOB NOT NULL UNIQUE CHECK (length(collection_id) = 16)) STRICT"
    SQL.execute_ connection "CREATE TABLE probe (id TEXT PRIMARY KEY, title TEXT NOT NULL) STRICT"
    SQL.execute_ connection "PRAGMA user_version = 1"

relaxedSchema :: Connection -> IO ()
relaxedSchema connection = SQL.execute_ connection "CREATE TABLE collection_meta (singleton ANY, collection_id ANY) STRICT"

insertIdentity :: Connection -> ByteString -> IO ()
insertIdentity connection bytes = SQL.execute connection "INSERT INTO collection_meta VALUES (1, ?)" (Only bytes)

readTitle :: Connection -> IO Text
readTitle connection = do
    rows <- SQL.query_ connection "SELECT title FROM probe WHERE id = 'same-id'"
    case rows of
        [Only title] -> pure title
        _ -> assertFailure "missing fixture article"

beginChange :: Connection -> IO ()
beginChange connection = do
    SQL.execute_ connection "BEGIN EXCLUSIVE"
    SQL.execute_ connection "UPDATE probe SET title = 'uncommitted'"

assertReleased :: CollectionDirectory -> Assertion
assertReleased root = do
    result <- withExistingCollection root collection $ \connection -> SQL.withExclusiveTransaction connection $ do
        readTitle connection >>= (@?= "original")
        SQL.execute_ connection "UPDATE probe SET title = 'released'"
    result @?= Right ()

malformed :: String -> (Connection -> IO ()) -> TestTree
malformed name setup = testCase name . withRoot $ \directory root -> do
    let path = collectionPath directory collection
    SQL.withConnection path setup
    assertRejected InvalidMetadata (withExistingCollection root collection) directory [path]

rejectedFile :: ByteString -> Assertion
rejectedFile contents = withRoot $ \directory root -> do
    let path = collectionPath directory collection
    ByteString.writeFile path contents
    assertRejected InvalidMetadata (withExistingCollection root collection) directory [path]

assertRejected :: OpenError -> ((Connection -> IO ()) -> IO (Either OpenError ())) -> FilePath -> [FilePath] -> Assertion
assertRejected expected open directory paths = do
    before <- snapshot
    open unexpectedCallback >>= (@?= Left expected)
    after <- snapshot
    after @?= before
  where
    snapshot = (,) . sort <$> listDirectory directory <*> mapM ByteString.readFile paths

assertUnavailable :: Either OpenError a -> Assertion
assertUnavailable = \case
    Left err -> err @?= Unavailable
    Right _ -> assertFailure "unavailable directory accepted"

assertSqlError :: IO () -> Assertion
assertSqlError action = do
    result <- try @SQLError action
    assertBool "expected SQLite constraint failure" $ case result of
        Left err -> sqlError err == SQLite.ErrorConstraint
        Right () -> False

requireRight :: (Show e) => Either e a -> IO a
requireRight = either (assertFailure . show) pure

await :: IO a -> IO a
await action = timeout 2000000 action >>= maybe (assertFailure "timed out") pure

unexpectedCallback :: Connection -> IO ()
unexpectedCallback _ = assertFailure "callback ran"
