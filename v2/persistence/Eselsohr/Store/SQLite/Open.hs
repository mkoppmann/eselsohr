{- |
Module      : Eselsohr.Store.SQLite.Open
Description : Opening existing collection databases

A t'CollectionDirectory' identifies an existing directory resolved by 'mkCollectionDirectory', and 'collectionPath' names the file of a collection inside it.
'withExistingCollection' opens the file named by a collection identifier, configures its connection, and checks its stored identity and schema version before invoking the callback.
Opening does not create or initialize missing databases; a missing collection file is reported as 'Missing', while links, other file types, and other filesystem failures are 'Unavailable'.

'withExistingFile' provides the same connection setup and cleanup for a literal absolute path, without the collection identity check.
Callbacks must finish their statements and leave connection closure to the opening function.
-}
module Eselsohr.Store.SQLite.Open
    ( -- * Collection opening
      CollectionDirectory
    , OpenError (..)
    , mkCollectionDirectory
    , collectionPath
    , withExistingCollection
    , checkCollection

      -- * Connection setup
    , withExistingFile
    ) where

import Control.Exception (IOException, SomeException, catch, mask, throwIO, try)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.IORef (newIORef)
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection (..), SQLData (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Eselsohr.Id (CollectionId, renderCollectionId)
import Eselsohr.Store.SQLite.Codec (catchConversionErrors, fromCollectionBytes)
import Eselsohr.Store.SQLite.Schema qualified as Schema
import System.Directory (canonicalizePath, makeAbsolute)
import System.FilePath (isAbsolute, (</>))
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getSymbolicLinkStatus, isDirectory, isRegularFile)

------------------------------------------------------------------------
-- Collection opening
------------------------------------------------------------------------

newtype CollectionDirectory = CollectionDirectory FilePath

data OpenError = Missing | Unavailable | InvalidMetadata | UnsupportedSchema | Busy | DatabaseFault
    deriving stock (Eq, Show)

-- | Resolve an existing collections directory once; opening never creates it.
mkCollectionDirectory :: FilePath -> IO (Either OpenError CollectionDirectory)
mkCollectionDirectory input
    | not (validPath input) = pure (Left Unavailable)
    | otherwise = filesystemCheck $ do
        root <- makeAbsolute input >>= canonicalizePath
        status <- getSymbolicLinkStatus root
        pure $ if isDirectory status then Right (CollectionDirectory root) else Left Unavailable

-- | The absolute path @\<collection\>.sqlite@ inside the directory.
collectionPath :: CollectionDirectory -> CollectionId -> FilePath
collectionPath (CollectionDirectory root) collection = root </> Text.unpack (renderCollectionId collection) <> ".sqlite"

withExistingCollection :: CollectionDirectory -> CollectionId -> (Connection -> IO a) -> IO (Either OpenError a)
withExistingCollection directory collection use = do
    let path = collectionPath directory collection
    regular <- checkCollectionFile path
    case regular of
        Left err -> pure (Left err)
        Right () -> join <$> withExistingFile path (\connection -> useCollection connection collection use)

-- | Require a regular file at the collection path, distinguishing a missing file from other failures.
checkCollectionFile :: FilePath -> IO (Either OpenError ())
checkCollectionFile path = check `catch` (pure . Left . classify)
  where
    check = do
        status <- getSymbolicLinkStatus path
        pure $ if isRegularFile status then Right () else Left Unavailable
    classify exception = if isDoesNotExistError exception then Missing else Unavailable

useCollection :: Connection -> CollectionId -> (Connection -> IO a) -> IO (Either OpenError a)
useCollection connection collection use = do
    checked <- checkCollection connection collection
    case checked of
        Left err -> pure (Left err)
        Right () -> Right <$> use connection

checkCollection :: Connection -> CollectionId -> IO (Either OpenError ())
checkCollection connection collection = do
    identity <- checkIdentity connection collection
    case identity of
        Left err -> pure (Left err)
        Right () -> do
            version <- Schema.checkSchemaVersion connection
            pure (first schemaError version)

schemaError :: Schema.SchemaError -> OpenError
schemaError = \case
    Schema.UnsupportedVersion -> UnsupportedSchema
    Schema.SchemaBusy -> Busy
    _ -> DatabaseFault

------------------------------------------------------------------------
-- Connection setup
------------------------------------------------------------------------

{- | Open an existing file for the callback and close the connection afterwards.
The absolute path is literal even when SQLite has URI filenames enabled globally.
Callbacks must finalize their statements and must not close or retain the connection.
SQLite may fall back to a read-only connection when read-write opening is unavailable.
-}
withExistingFile :: FilePath -> (Connection -> IO a) -> IO (Either OpenError a)
withExistingFile path use
    | not (isAbsolute path && validPath path) = pure (Left Unavailable)
    | otherwise = mask $ \restore -> do
        acquired <- openConnection path
        case acquired of
            Left err -> pure (Left err)
            Right connection -> do
                outcome <- try @SomeException $ restore (useConfiguredConnection connection use)
                closeConnection connection outcome

openConnection :: FilePath -> IO (Either OpenError Connection)
openConnection path = databaseCheck $ do
    counter <- newIORef 0
    database <- SQLite.open2 (Text.pack path) [SQLite.SQLOpenReadWrite, SQLite.SQLOpenPrivateCache, SQLite.SQLOpenNoFollow] SQLite.SQLVFSDefault
    pure (Connection database counter)

useConfiguredConnection :: Connection -> (Connection -> IO a) -> IO (Either OpenError a)
useConfiguredConnection connection use = do
    configured <- configure connection
    case configured of
        Left err -> pure (Left err)
        Right () -> Right <$> use connection

closeConnection :: Connection -> Either SomeException (Either OpenError a) -> IO (Either OpenError a)
closeConnection connection outcome = do
    closed <- databaseCheck (SQL.close connection)
    case outcome of
        Left exception -> throwIO exception
        Right (Left err) -> pure (Left err)
        Right (Right value) -> pure (value <$ closed)

configure :: Connection -> IO (Either OpenError ())
configure connection = fmap join . databaseCheck $ do
    SQL.execute_ connection "PRAGMA foreign_keys = ON"
    SQL.execute_ connection "PRAGMA trusted_schema = OFF"
    SQL.execute_ connection "PRAGMA busy_timeout = 0"

    foreignKeys <- SQL.query_ connection "PRAGMA foreign_keys"
    trustedSchema <- SQL.query_ connection "PRAGMA trusted_schema"
    busyTimeout <- SQL.query_ connection "PRAGMA busy_timeout"
    pure (validateSettings foreignKeys trustedSchema busyTimeout)

validateSettings :: [[SQLData]] -> [[SQLData]] -> [[SQLData]] -> Either OpenError ()
validateSettings [[SQLInteger 1]] [[SQLInteger 0]] [[SQLInteger 0]] = Right ()
validateSettings _ _ _ = Left DatabaseFault

------------------------------------------------------------------------
-- Opening checks
------------------------------------------------------------------------

checkIdentity :: Connection -> CollectionId -> IO (Either OpenError ())
checkIdentity connection expected = do
    result <- databaseCheck . catchConversionErrors $ readIdentity connection
    pure $ case result of
        Left err -> Left err
        Right (Left _) -> Left InvalidMetadata
        Right (Right rows) -> validateIdentity expected rows

readIdentity :: Connection -> IO [[SQLData]]
readIdentity connection = do
    objects <- SQL.query_ connection "SELECT type FROM sqlite_schema WHERE name = 'collection_meta'"
    if objects /= [[SQLText "table"]]
        then pure []
        else
            SQL.query_ connection "SELECT singleton, collection_id FROM collection_meta LIMIT 2" `catch` \exception ->
                if SQLite.sqlError exception == SQLite.ErrorError then pure [] else throwIO exception

validateIdentity :: CollectionId -> [[SQLData]] -> Either OpenError ()
validateIdentity expected [[SQLInteger 1, SQLBlob bytes]]
    | fromCollectionBytes bytes == Right expected = Right ()
validateIdentity _ _ = Left InvalidMetadata

validPath :: FilePath -> Bool
validPath path = not (null path) && all (\c -> c /= '\0' && (c < '\xD800' || c > '\xDFFF')) path

filesystemCheck :: IO (Either OpenError a) -> IO (Either OpenError a)
filesystemCheck action = action `catch` \(_ :: IOException) -> pure (Left Unavailable)

databaseCheck :: IO a -> IO (Either OpenError a)
databaseCheck action = (Right <$> action) `catch` \exception -> pure . Left . classify $ SQLite.sqlError exception
  where
    classify = \case
        SQLite.ErrorBusy -> Busy
        SQLite.ErrorLocked -> Busy
        SQLite.ErrorCan'tOpen -> Unavailable
        SQLite.ErrorNotADatabase -> InvalidMetadata
        SQLite.ErrorCorrupt -> InvalidMetadata
        _ -> DatabaseFault
