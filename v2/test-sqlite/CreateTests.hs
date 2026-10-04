{- |
Module      : CreateTests
Description : Collection creation tests

'createTests' creates collections in temporary directories and checks the file mode, the stored identity and schema, the single owner grant, that storage holds only the bearer's hash, and that the returned bearer reads the new collection through the production runners.
Failure cases check that an existing path is never opened or replaced and that a failure inside the creation transaction leaves no file.
-}
module CreateTests (createTests) where

import Control.Monad (forM_)
import Data.Bits ((.&.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64
import Data.Int (Int64)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Only (..), SQLData (..))
import Database.SQLite.Simple qualified as SQL
import Effectful (runEff)
import Eselsohr.Capability (GrantSnapshot (..))
import Eselsohr.Id (CollectionId)
import Eselsohr.Store (ArticleSelection (..), listArticles)
import Eselsohr.Store.Runtime (Binding (..), runArticleReader)
import Eselsohr.Store.SQLite.Bearer (Bearer, bearerCollection, bearerHash, newBearer, renderBearer)
import Eselsohr.Store.SQLite.Codec (StoredGrant (..), fromTimestamp)
import Eselsohr.Store.SQLite.Create
import Eselsohr.Store.SQLite.Open (CollectionDirectory, collectionPath, mkCollectionDirectory, withExistingCollection)
import SqliteFixtures
import System.Directory (doesPathExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, fileMode, getFileStatus)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

createTests :: TestTree
createTests =
    testGroup
        "Collection creation"
        [ testCase "a new collection has its identity, schema, and one owner grant" . withDirectory $ \_ root -> do
            bearer <- requireRight =<< createCollection root
            rows <- requireRight =<< withExistingCollection root (bearerCollection bearer) grantRows
            rows @?= ([[SQLBlob (bearerHash bearer), SQLNull, SQLNull, SQLNull, SQLInteger 0, SQLNull]], [Only 8])
        , testCase "the file is readable and writable by its owner only" . withDirectory $ \_ root -> do
            bearer <- requireRight =<< createCollection root
            status <- getFileStatus (collectionPath root (bearerCollection bearer))
            fileMode status .&. 0o777 @?= 0o600
        , testCase "the database holds no copy of the secret" . withDirectory $ \_ root -> do
            bearer <- requireRight =<< createCollection root
            let encoded = encodeUtf8 . Text.takeWhileEnd (/= '.') $ renderBearer bearer
            secret <- requireRight (Base64.decodeUnpadded encoded)
            contents <- ByteString.readFile (collectionPath root (bearerCollection bearer))
            ByteString.length secret @?= 32
            assertBool "raw secret stored" . not $ secret `ByteString.isInfixOf` contents
            assertBool "encoded secret stored" . not $ encoded `ByteString.isInfixOf` contents
            assertBool "hash missing" (bearerHash bearer `ByteString.isInfixOf` contents)
        , testCase "the owner bearer reads the new collection through the production runners" . withDirectory $ \_ root -> do
            bearer <- requireRight =<< createCollection root
            articles <- runEff . runArticleReader (Binding root bearer (pure now)) $ listArticles AllArticles
            assertSame (Right []) articles
        , testCase "two creations differ in identifiers and secrets" . withDirectory $ \directory root -> do
            first' <- requireRight =<< createCollection root
            second' <- requireRight =<< createCollection root
            assertBool "same collection" (bearerCollection first' /= bearerCollection second')
            assertBool "same hash" (bearerHash first' /= bearerHash second')
            assertBool "same secret" (Text.takeWhileEnd (/= '.') (renderBearer first') /= Text.takeWhileEnd (/= '.') (renderBearer second'))
            firstRoot <- rootIdentifierOf root first'
            secondRoot <- rootIdentifierOf root second'
            assertBool "same root grant" (firstRoot /= secondRoot)
            files <- listDirectory directory
            length files @?= 2
        , testCase "an existing file or link at the path is left unchanged" . withDirectory $ \directory root -> do
            let path = collectionPath root collection
            ByteString.writeFile path "keep"
            createCollectionWith root collection (validRoot collection) >>= (@?= Left CollectionExists)
            ByteString.readFile path >>= (@?= "keep")

            let target = directory </> "target"
                linked = collectionPath root otherCollection
            ByteString.writeFile target "target"
            createSymbolicLink target linked
            createCollectionWith root otherCollection (validRoot otherCollection) >>= (@?= Left CollectionExists)
            ByteString.readFile target >>= (@?= "target")
        , testCase "a failure inside the creation transaction leaves no file" . withDirectory $ \directory root ->
            forM_ invalidRoots $ \invalid -> do
                createCollectionWith root collection invalid >>= (@?= Left CreateFault)
                doesPathExist (collectionPath root collection) >>= (@?= False)
                listDirectory directory >>= (@?= [])
        ]
  where
    invalidRoots =
        [ (validRoot collection){storedGrant = (storedGrant (validRoot collection)){grantDepth = 1}}
        , (validRoot collection){storedTokenHash = ByteString.replicate 31 1}
        , (validRoot collection){storedGrant = (storedGrant (validRoot collection)){grantExpiresAt = Just (fromTimestamp 0)}}
        ]

withDirectory :: (FilePath -> CollectionDirectory -> Assertion) -> Assertion
withDirectory use = withSystemTempDirectory "eselsohr-create" $ \directory -> do
    root <- requireRight =<< mkCollectionDirectory directory
    use directory root

-- | An owner grant for a collection with a fixed identifier and secret.
validRoot :: CollectionId -> StoredGrant
validRoot owner = ownerGrant owner rootIdentifier (fixedBearer owner)

fixedBearer :: CollectionId -> Bearer
fixedBearer owner = either (error . show) id (newBearer owner (ByteString.replicate 32 9))

-- | Every stored grant's fields other than its identifier, and the number of permission rows.
grantRows :: SQL.Connection -> IO ([[SQLData]], [Only Int])
grantRows connection = do
    grants <- SQL.query_ connection "SELECT token_hash, parent_id, scope_article_id, expires_at, depth, petname FROM capability"
    permissions <- SQL.query_ connection "SELECT count(*) FROM capability_permission"
    pure (grants, permissions)

rootIdentifierOf :: CollectionDirectory -> Bearer -> IO [[SQLData]]
rootIdentifierOf root bearer =
    requireRight =<< withExistingCollection root (bearerCollection bearer) (`SQL.query_` "SELECT id FROM capability")

now :: UTCTime
now = fromTimestamp (1800000000 * 1000000 :: Int64)
