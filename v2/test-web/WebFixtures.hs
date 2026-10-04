{- |
Module      : WebFixtures
Description : Real collections and in-process requests for web tests

'withCollections' resolves a fresh temporary collections directory, and 'newCollection' creates a collection in it through the production creation code, returning its identifier, file, and owner bearer text.
'addGrant', 'addArticle', 'removeGrant', and 'corruptArticle' change a collection directly through SQLite, outside the protected operations, to set up delegated grants, articles, revocations, and malformed rows.

'runApplication' calls a WAI application in process and collects its status, headers, and body, so tests can exercise the whole application, including its middleware, without a listener.
-}
module WebFixtures
    ( -- * Collections
      Collection (..)
    , withCollections
    , newCollection
    , addGrant
    , addArticle
    , removeGrant
    , corruptArticle
    , holdExclusive

      -- * Requests
    , Exchange (..)
    , runApplication
    , pageRequest
    , formRequest
    , ignoreMessages
    , capturingMessages
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word8)
import Database.SQLite.Simple (Connection, Only (..))
import Database.SQLite.Simple qualified as SQL
import Eselsohr.Article (mkTitle, newArticle)
import Eselsohr.Capability (GrantSnapshot (..), Permissions, Scope)
import Eselsohr.Id (ArticleId, CollectionId, parseArticleId)
import Eselsohr.Store.Runtime (CollectionDirectory, resolveDirectory)
import Eselsohr.Store.SQLite.Bearer (bearerCollection, bearerHash, newBearer, parseBearer, renderBearer)
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Create (createCollection)
import Eselsohr.Store.SQLite.Open (collectionPath, withExistingCollection)
import Eselsohr.WebUrl (parseWebUrl)
import Network.HTTP.Types (ResponseHeaders, statusCode)
import Network.Wai (Application, Request, RequestBodyLength (..), defaultRequest, pathInfo, requestBodyLength, requestHeaders, requestMethod, responseHeaders, responseStatus, responseToStream, setRequestBodyChunks)
import Network.Wai.Internal (ResponseReceived (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit (assertFailure)

------------------------------------------------------------------------
-- Collections
------------------------------------------------------------------------

data Collection = Collection
    { collectionIdentifier :: CollectionId
    , collectionDirectory :: CollectionDirectory
    , ownerToken :: Text
    }

-- | Resolve a fresh empty collections directory for one test.
withCollections :: (FilePath -> CollectionDirectory -> IO a) -> IO a
withCollections use = withSystemTempDirectory "eselsohr-web" $ \path ->
    resolveDirectory path >>= maybe (assertFailure "the temporary directory did not resolve") (use path)

newCollection :: CollectionDirectory -> IO Collection
newCollection directory = do
    owner <- createCollection directory >>= requireRight
    pure (Collection (bearerCollection owner) directory (renderBearer owner))

-- | Add a child of the root grant and return its bearer text; the seed byte determines its identifier and secret.
addGrant :: Collection -> Word8 -> Permissions -> Scope -> UTCTime -> IO Text
addGrant collection seed permissions scope deadline = do
    bearer <- requireRight (newBearer (collectionIdentifier collection) (ByteString.replicate 32 seed))
    identifier <- requireRight (fromCapabilityBytes (uuidBytes seed))
    withDatabase collection $ \connection -> do
        roots <- SQL.query_ connection "SELECT id FROM capability WHERE parent_id IS NULL"
        root <- case roots of
            [Only bytes] -> requireRight (fromCapabilityBytes bytes)
            _ -> assertFailure "expected one root grant"
        let grant = StoredGrant (GrantSnapshot identifier (Just root) scope permissions (Just deadline) 1) (bearerHash bearer) Nothing
        (row, rows) <- requireRight (toGrantRows (collectionIdentifier collection) grant)
        SQL.execute connection "INSERT INTO capability (id, token_hash, parent_id, scope_article_id, expires_at, depth, petname) VALUES (?, ?, ?, ?, ?, ?, ?)" row
        SQL.executeMany connection "INSERT INTO capability_permission (capability_id, permission) VALUES (?, ?)" rows
    pure (renderBearer bearer)

-- | Add an unread article at revision zero.
addArticle :: Collection -> Text -> Text -> Text -> UTCTime -> IO ArticleId
addArticle collection identifierText titleText urlText created = do
    identifier <- requireRight (parseArticleId identifierText)
    title <- requireRight (mkTitle titleText)
    url <- requireRight (parseWebUrl urlText)
    row <- requireRight (toArticleRow (StoredArticle (newArticle identifier title url created) 0))
    withDatabase collection $ \connection -> insertArticleRow connection row
    pure identifier

-- | Add an article row whose title is not canonical, so reading it is corruption.
corruptArticle :: Collection -> Text -> UTCTime -> IO ()
corruptArticle collection identifierText created = do
    identifier <- requireRight (parseArticleId identifierText)
    title <- requireRight (mkTitle "Corrupt")
    url <- requireRight (parseWebUrl "https://example.com/corrupt")
    row <- requireRight (toArticleRow (StoredArticle (newArticle identifier title url created) 0))
    withDatabase collection $ \connection -> insertArticleRow connection row{rowTitle = " Corrupt "}

-- | Delete the grant of a bearer, as a revocation does.
removeGrant :: Collection -> Text -> IO ()
removeGrant collection token = do
    bearer <- requireRight (parseBearer token)
    withDatabase collection $ \connection ->
        SQL.execute connection "DELETE FROM capability WHERE token_hash = ?" (Only (bearerHash bearer))

-- | Run an action while another connection holds an exclusive lock on the collection.
holdExclusive :: Collection -> IO a -> IO a
holdExclusive collection action =
    SQL.withConnection (collectionPath (collectionDirectory collection) (collectionIdentifier collection)) $ \holder -> do
        SQL.execute_ holder "BEGIN EXCLUSIVE"
        result <- action
        SQL.execute_ holder "ROLLBACK"
        pure result

withDatabase :: Collection -> (Connection -> IO ()) -> IO ()
withDatabase collection use =
    withExistingCollection (collectionDirectory collection) (collectionIdentifier collection) use >>= requireRight

insertArticleRow :: Connection -> ArticleRow -> IO ()
insertArticleRow connection = SQL.execute connection "INSERT INTO article (id, title, url, state, created_at, revision) VALUES (?, ?, ?, ?, ?, ?)"

-- | 16 bytes of the seed with the UUIDv4 version and variant bits set.
uuidBytes :: Word8 -> ByteString
uuidBytes seed = ByteString.pack ([seed, seed, seed, seed, seed, seed, 0x40, seed, 0x80] <> replicate 7 seed)

requireRight :: (Show e) => Either e a -> IO a
requireRight = either (assertFailure . show) pure

------------------------------------------------------------------------
-- Requests
------------------------------------------------------------------------

data Exchange = Exchange
    { exchangeStatus :: Int
    , exchangeHeaders :: ResponseHeaders
    , exchangeBody :: ByteString
    }

runApplication :: Application -> Request -> IO Exchange
runApplication app request = do
    result <- newIORef Nothing
    _ <- app request $ \response -> do
        body <- collectBody response
        writeIORef result (Just (Exchange (statusCode (responseStatus response)) (responseHeaders response) body))
        pure ResponseReceived
    readIORef result >>= maybe (assertFailure "the application did not respond") pure
  where
    collectBody response = do
        let (_, _, withBody) = responseToStream response
        chunks <- newIORef mempty
        withBody $ \stream -> stream (\chunk -> modifyIORef' chunks (<> chunk)) (pure ())
        LazyByteString.toStrict . toLazyByteString <$> readIORef chunks

-- | A GET request for the page of a bearer.
pageRequest :: Text -> Request
pageRequest token = defaultRequest{pathInfo = ["b", token]}

-- | A POST request to an action of a bearer with the given form body, delivered in one chunk.
formRequest :: Text -> Text -> ByteString -> IO Request
formRequest token action body = do
    remaining <- newIORef [body]
    let next = atomicModifyIORef' remaining $ \case
            chunk : rest -> (rest, chunk)
            [] -> ([], "")
    pure . setRequestBodyChunks next $
        defaultRequest
            { requestMethod = "POST"
            , pathInfo = ["b", token, action]
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            , requestBodyLength = KnownLength (fromIntegral (ByteString.length body))
            }

ignoreMessages :: Text -> IO ()
ignoreMessages _ = pure ()

-- | A message sink and a way to read what it received.
capturingMessages :: IO (Text -> IO (), IO [Text])
capturingMessages = do
    messages <- newIORef []
    pure (\message -> atomicModifyIORef' messages (\logged -> (logged <> [message], ())), readIORef messages)
