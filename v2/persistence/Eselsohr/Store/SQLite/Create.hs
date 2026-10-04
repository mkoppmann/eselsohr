{- |
Module      : Eselsohr.Store.SQLite.Create
Description : Creating a collection with its owner grant

'createCollection' creates a new collection in a resolved collections directory and returns the owner t'Bearer'.
It draws the collection identifier, the root grant identifier, and the 32-byte secret from the operating system's random generator through @crypton@; identifiers are UUIDv4 values made from 16 random bytes.

'createCollectionWith' performs the creation for a given collection identifier and root grant, so tests can choose them; 'ownerGrant' builds the root grant that 'createCollection' uses.
It creates @\<collection\>.sqlite@ exclusively with mode 0600, so it never opens, follows, or replaces an existing path.
In one immediate transaction it installs the schema and identity and inserts the root grant with its permissions, so a collection never exists without its owner grant.
Any failure or exception after the file exists removes the file, so a failed creation leaves no collection behind.

Storage keeps only the bearer's hash; the token exists only in the returned value.
Errors carry no identifier, path, or secret.
-}
module Eselsohr.Store.SQLite.Create
    ( CreateError (..)
    , createCollection
    , createCollectionWith
    , ownerGrant
    ) where

import Control.Exception (IOException, catch, mask, onException)
import Crypto.Random (getRandomBytes)
import Data.Bifunctor (first)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Eselsohr.Capability (GrantSnapshot (..), Scope (..))
import Eselsohr.Id (CapabilityId, CollectionId)
import Eselsohr.Store.SQLite.Bearer (Bearer, bearerHash, newBearer)
import Eselsohr.Store.SQLite.Codec (StoredGrant (..), fromCapabilityBytes, fromCollectionBytes, toGrantRows)
import Eselsohr.Store.SQLite.Open (CollectionDirectory, collectionPath, withExistingFile)
import Eselsohr.Store.SQLite.Schema (SchemaError (..), installSchema, withSchemaTransaction)
import System.Directory (removeFile)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags, openFd)

-- | 'CollectionExists' reports an existing path, 'CreateUnavailable' a directory that cannot hold the file, and 'CreateFault' any later failure.
data CreateError = CollectionExists | CreateUnavailable | CreateFault
    deriving stock (Eq, Show)

createCollection :: CollectionDirectory -> IO (Either CreateError Bearer)
createCollection directory = do
    collectionBytes <- randomUuidBytes
    rootBytes <- randomUuidBytes
    secret <- getRandomBytes 32
    case prepareOwner collectionBytes rootBytes secret of
        Left err -> pure (Left err)
        Right (collection, root, bearer) -> fmap (bearer <$) (createCollectionWith directory collection root)

prepareOwner :: ByteString -> ByteString -> ByteString -> Either CreateError (CollectionId, StoredGrant, Bearer)
prepareOwner collectionBytes rootBytes secret = do
    collection <- fault (fromCollectionBytes collectionBytes)
    root <- fault (fromCapabilityBytes rootBytes)
    bearer <- fault (newBearer collection secret)
    pure (collection, ownerGrant collection root bearer, bearer)
  where
    fault :: Either e a -> Either CreateError a
    fault = first (const CreateFault)

-- | The root grant: every permission on the whole collection, no parent or expiration, depth zero, and no petname.
ownerGrant :: CollectionId -> CapabilityId -> Bearer -> StoredGrant
ownerGrant collection identifier bearer =
    StoredGrant
        { storedGrant = GrantSnapshot identifier Nothing (WholeCollection collection) (Set.fromList [minBound .. maxBound]) Nothing 0
        , storedTokenHash = bearerHash bearer
        , storedPetname = Nothing
        }

-- | 16 random bytes with the UUIDv4 version and variant bits set.
randomUuidBytes :: IO ByteString
randomUuidBytes = do
    bytes <- getRandomBytes 16
    pure . ByteString.pack . zipWith mark [0 :: Int ..] $ ByteString.unpack bytes
  where
    mark 6 byte = (byte .&. 0x0f) .|. 0x40
    mark 8 byte = (byte .&. 0x3f) .|. 0x80
    mark _ byte = byte

------------------------------------------------------------------------
-- Creating the file
------------------------------------------------------------------------

createCollectionWith :: CollectionDirectory -> CollectionId -> StoredGrant -> IO (Either CreateError ())
createCollectionWith directory collection root = mask $ \restore -> do
    let path = collectionPath directory collection
    created <- createExclusive path
    case created of
        Left err -> pure (Left err)
        Right () -> do
            result <- restore (initialize path collection root) `onException` discard path
            case result of
                Right () -> pure (Right ())
                Left err -> Left err <$ discard path

-- | Create an empty file with mode 0600, failing if anything, including a symbolic link, exists at the path.
createExclusive :: FilePath -> IO (Either CreateError ())
createExclusive path = (Right () <$ (openFd path WriteOnly flags >>= closeFd)) `catch` (pure . Left . classify)
  where
    flags = defaultFileFlags{exclusive = True, creat = Just (unionFileModes ownerReadMode ownerWriteMode)}
    classify exception = if isAlreadyExistsError exception then CollectionExists else CreateUnavailable

-- | Remove a file this creation made; a failure to remove it leaves the original error in place.
discard :: FilePath -> IO ()
discard path = removeFile path `catch` \(_ :: IOException) -> pure ()

------------------------------------------------------------------------
-- Initializing the database
------------------------------------------------------------------------

initialize :: FilePath -> CollectionId -> StoredGrant -> IO (Either CreateError ())
initialize path collection root = do
    opened <- withExistingFile path $ \connection -> withSchemaTransaction connection $ do
        installed <- installSchema connection collection
        case installed of
            Left err -> pure (Left err)
            Right () -> insertRoot connection collection root
    pure $ case opened of
        Left _ -> Left CreateUnavailable
        Right (Left _) -> Left CreateFault
        Right (Right ()) -> Right ()

insertRoot :: Connection -> CollectionId -> StoredGrant -> IO (Either SchemaError ())
insertRoot connection collection root = case toGrantRows collection root of
    Left _ -> pure (Left SchemaFault)
    Right (row, permissions) -> do
        SQL.execute connection "INSERT INTO capability (id, token_hash, parent_id, scope_article_id, expires_at, depth, petname) VALUES (?, ?, ?, ?, ?, ?, ?)" row
        SQL.executeMany connection "INSERT INTO capability_permission (capability_id, permission) VALUES (?, ?)" permissions
        pure (Right ())
