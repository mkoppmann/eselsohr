{- |
Module      : SqliteFixtures
Description : Shared database fixtures and SQLite test assertions

'withEmptyDatabase' supplies an existing empty file and a configured connection; 'withDatabase' also installs the initial collection schema.
Both helpers keep temporary files and connections within the test callback's lifetime.

Shared article and grant values, insertion helpers, result assertions, and bounded waits support the SQLite test groups.
-}
module SqliteFixtures
    ( -- * Fixture values
      collection
    , otherCollection
    , collectionBytes
    , articleIdentifier
    , rootIdentifier
    , childIdentifier
    , articleFixture
    , rootFixture
    , childFixture

      -- * Database setup
    , withEmptyDatabase
    , withEmptyDatabaseFor
    , withDatabase
    , insertArticle
    , insertGrant

      -- * Assertions and failures
    , requireRight
    , assertSame
    , assertLeft
    , await
    , CallbackFailure (..)
    ) where

import Control.Exception (Exception)
import Data.ByteString qualified as ByteString
import Data.Set qualified as Set
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Eselsohr.Article
import Eselsohr.Capability
import Eselsohr.Id
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Open
import Eselsohr.Store.SQLite.Schema
import Eselsohr.WebUrl (parseWebUrl)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))

------------------------------------------------------------------------
-- Fixture values
------------------------------------------------------------------------

collection, otherCollection :: CollectionId
collection = value (parseCollectionId "123e4567-e89b-42d3-a456-426614174000")
otherCollection = value (parseCollectionId "123e4567-e89b-42d3-a456-426614174001")

collectionBytes :: ByteString.ByteString
collectionBytes = ByteString.pack [0x12, 0x3e, 0x45, 0x67, 0xe8, 0x9b, 0x42, 0xd3, 0xa4, 0x56, 0x42, 0x66, 0x14, 0x17, 0x40, 0x00]

articleIdentifier :: ArticleId
articleIdentifier = value (parseArticleId "123e4567-e89b-42d3-a456-426614174002")

rootIdentifier, childIdentifier :: CapabilityId
rootIdentifier = value (parseCapabilityId "123e4567-e89b-42d3-a456-426614174003")
childIdentifier = value (parseCapabilityId "123e4567-e89b-42d3-a456-426614174004")

articleFixture :: StoredArticle
articleFixture = StoredArticle (newArticle articleIdentifier (value (mkTitle "A title")) (value (parseWebUrl "https://example.com/")) (fromTimestamp 1234567)) 0

rootFixture, childFixture :: StoredGrant
rootFixture = StoredGrant (GrantSnapshot rootIdentifier Nothing (WholeCollection collection) (Set.fromList [minBound .. maxBound]) Nothing 0) (ByteString.replicate 32 1) Nothing
childFixture = StoredGrant (GrantSnapshot childIdentifier (Just rootIdentifier) (OneArticle collection articleIdentifier) (Set.singleton ReadArticles) (Just (fromTimestamp (-1))) 1) (ByteString.replicate 32 2) (Just "phone")

------------------------------------------------------------------------
-- Database setup
------------------------------------------------------------------------

withEmptyDatabase :: (FilePath -> CollectionDirectory -> Connection -> IO a) -> IO a
withEmptyDatabase = withEmptyDatabaseFor collection

withEmptyDatabaseFor :: CollectionId -> (FilePath -> CollectionDirectory -> Connection -> IO a) -> IO a
withEmptyDatabaseFor owner use = withSystemTempDirectory "eselsohr-schema" $ \directory -> do
    root <- requireRight =<< mkCollectionDirectory directory
    let path = directory </> Text.unpack (renderCollectionId owner) <> ".sqlite"
    ByteString.writeFile path ByteString.empty

    requireRight =<< withExistingFile path (use path root)

withDatabase :: (Connection -> IO a) -> IO a
withDatabase use = withEmptyDatabase $ \_ _ connection -> do
    requireRight =<< initializeSchema connection collection
    use connection

insertArticle :: Connection -> StoredArticle -> IO ()
insertArticle connection article = do
    row <- requireRight (toArticleRow article)
    SQL.execute connection "INSERT INTO article (id, title, url, state, created_at, revision) VALUES (?, ?, ?, ?, ?, ?)" row

insertGrant :: Connection -> CollectionId -> StoredGrant -> IO ()
insertGrant connection owner grant = do
    (row, permissions) <- requireRight (toGrantRows owner grant)
    SQL.execute connection "INSERT INTO capability (id, token_hash, parent_id, scope_article_id, expires_at, depth, petname) VALUES (?, ?, ?, ?, ?, ?, ?)" row
    SQL.executeMany connection "INSERT INTO capability_permission (capability_id, permission) VALUES (?, ?)" permissions

------------------------------------------------------------------------
-- Assertions and failures
------------------------------------------------------------------------

requireRight :: (Show e) => Either e a -> IO a
requireRight = either (assertFailure . show) pure

assertSame :: (Eq a) => a -> a -> Assertion
assertSame expected actual = assertBool "values differ" (expected == actual)

assertLeft :: (Eq e, Show e) => e -> Either e a -> Assertion
assertLeft expected = either (@?= expected) (const (assertFailure "unexpected success"))

await :: IO a -> IO a
await action = timeout 2000000 action >>= maybe (assertFailure "timed out") pure

data CallbackFailure = CallbackFailure deriving stock (Eq, Show)
instance Exception CallbackFailure

value :: (Show e) => Either e a -> a
value = either (error . show) id
