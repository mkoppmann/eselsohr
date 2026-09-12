{- |
Module      : Eselsohr.Store.SQLite.Bearer
Description : Bearer token parsing and stored grant lookup

A t'Bearer' represents a canonical token containing a collection identifier and a secret in the @v2.\<collection\>.\<secret\>@ format.
'parseBearer' validates the encoded fields and derives the hash used for lookup; 'renderBearer' reproduces the original token.

'lookupBearer' requires an active transaction, verifies the collection identity and schema version, and retrieves the matching grant and its permissions by hash.
It returns 'Nothing' when the hash has no match.
Ancestor, expiration, and operation checks belong to the protected article operations.
-}
module Eselsohr.Store.SQLite.Bearer
    ( -- * Bearer format
      Bearer
    , BearerError (..)
    , parseBearer
    , renderBearer
    , bearerCollection

      -- * Grant lookup
    , LookupError (..)
    , lookupBearer
    ) where

import Control.Exception (catch)
import Control.Monad (join, unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Id (CollectionId)
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Open qualified as Open

------------------------------------------------------------------------
-- Bearer format
------------------------------------------------------------------------

data Bearer = Bearer Text CollectionId !ByteString

data BearerError = InvalidBearer
    deriving stock (Eq, Show)

parseBearer :: Text -> Either BearerError Bearer
parseBearer input
    | Text.compareLength input 69 /= EQ = Left InvalidBearer
    | otherwise = case Text.splitOn "." input of
        ["v2", collectionText, secretText] -> do
            collectionBytes <- decodeField 22 16 collectionText
            collection <- first (const InvalidBearer) $ fromCollectionBytes collectionBytes
            secret <- decodeField 43 32 secretText
            let digest = SHA256.hash ("eselsohr:bearer:v2\0" <> collectionBytes <> secret)
            pure (Bearer input collection digest)
        _ -> Left InvalidBearer
  where
    decodeField encodedLength decodedLength encoded = do
        unless (Text.length encoded == encodedLength) (Left InvalidBearer)
        let bytes = encodeUtf8 encoded
        decoded <- first (const InvalidBearer) $ Base64.decodeUnpadded bytes
        unless (ByteString.length decoded == decodedLength && Base64.encodeUnpadded decoded == bytes) (Left InvalidBearer)
        pure decoded

renderBearer :: Bearer -> Text
renderBearer (Bearer original _ _) = original

bearerCollection :: Bearer -> CollectionId
bearerCollection (Bearer _ collection _) = collection

------------------------------------------------------------------------
-- Grant lookup
------------------------------------------------------------------------

data LookupError = TransactionRequired | CorruptData | LookupBusy | LookupFault
    deriving stock (Eq, Show)

{- | Find the grant and its permissions using the bearer's hash.
Requires an active transaction and checks the collection identity, schema version, and stored grant fields.
Does not check the parent chain, expiration, or permission to perform an operation.
-}
lookupBearer :: Connection -> Bearer -> IO (Either LookupError (Maybe StoredGrant))
lookupBearer connection (Bearer _ collection digest) = fmap join . lookupCheck $ do
    ready <- checkLookup connection collection
    case ready of
        Left err -> pure (Left err)
        Right () -> findStoredGrant connection collection digest

checkLookup :: Connection -> CollectionId -> IO (Either LookupError ())
checkLookup connection collection = do
    autocommit <- Direct.getAutoCommit (connectionHandle connection)
    if autocommit
        then pure (Left TransactionRequired)
        else do
            result <- Open.checkCollection connection collection
            pure (first collectionError result)

collectionError :: Open.OpenError -> LookupError
collectionError = \case
    Open.InvalidMetadata -> CorruptData
    Open.Busy -> LookupBusy
    _ -> LookupFault

findStoredGrant :: Connection -> CollectionId -> ByteString -> IO (Either LookupError (Maybe StoredGrant))
findStoredGrant connection collection digest = do
    rows <- SQL.query connection "SELECT id, token_hash, parent_id, scope_article_id, expires_at, depth, petname FROM capability WHERE token_hash = ? LIMIT 2" (Only digest)
    case rows of
        [] -> pure (Right Nothing)
        [row] -> do
            permissions <- SQL.query connection "SELECT capability_id, permission FROM capability_permission WHERE capability_id = ? LIMIT 9" (Only (rowGrantId row))
            pure . fmap Just . first (const CorruptData) $ fromGrantRows collection row permissions
        _ -> pure (Left CorruptData)

lookupCheck :: IO a -> IO (Either LookupError a)
lookupCheck action =
    fmap (first (const CorruptData)) (catchConversionErrors action) `catch` \err ->
        pure . Left $ case SQL.sqlError err of
            SQLite.ErrorBusy -> LookupBusy
            SQLite.ErrorLocked -> LookupBusy
            _ -> LookupFault
