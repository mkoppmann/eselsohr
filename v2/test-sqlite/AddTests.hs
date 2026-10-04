{- |
Module      : AddTests
Description : Protected article addition tests

'addTests' checks that adding requires AddArticles on the whole collection inside the write transaction, inserts an unread article at revision zero created at the authorization instant in whole microseconds, and stores at most one article per URL with the same result for new and stored URLs.
Failure cases cover a one-article grant, a primary-key collision from a fixed generator, and rollback after an exception and after cancellation.
-}
module AddTests (addTests) where

import BearerTests (goldenDigest, goldenToken, zeroDigest, zeroToken)
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime (..), picosecondsToDiffTime)
import Database.SQLite.Simple (Connection (..), Only (..))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3.Direct qualified as Direct
import Eselsohr.Article (Article (..), ArticleState (..), Title, mkTitle)
import Eselsohr.Capability
import Eselsohr.Id (ArticleId, parseArticleId)
import Eselsohr.Store.SQLite.Articles
import Eselsohr.Store.SQLite.Bearer (Bearer, parseBearer)
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Schema (initializeSchema)
import Eselsohr.WebUrl (WebUrl, parseWebUrl)
import SqliteFixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

addTests :: TestTree
addTests =
    testGroup
        "Protected addition"
        [ testCase "the owner adds an unread article at revision zero" . withAddDatabase $ \connection owner -> do
            addArticle connection owner (pure instant) (pure newIdentifier) newUrl newTitle >>= (@?= Right ())
            stored <- storedArticles connection
            assertSame [StoredArticle (Article newIdentifier newTitle newUrl Unread instant) 0] stored
        , testCase "the creation time is the authorization instant in whole microseconds" . withAddDatabase $ \connection owner -> do
            let precise = UTCTime (utctDay instant) (picosecondsToDiffTime 1234567891)
            addArticle connection owner (pure precise) (pure newIdentifier) newUrl newTitle >>= (@?= Right ())
            stored <- storedArticles connection
            map (articleCreatedAt . storedArticle) stored @?= [UTCTime (utctDay instant) (picosecondsToDiffTime 1234000000)]
        , testCase "an add-only contributor may add, a one-article grant may not" . withAddDatabase $ \connection _ -> do
            insertArticle connection articleFixture
            insertGrant connection collection (contributor (WholeCollection collection))
            child <- bearerFor zeroToken
            addArticle connection child (pure instant) (pure newIdentifier) newUrl newTitle >>= (@?= Right ())

            SQL.execute connection "DELETE FROM capability WHERE token_hash = ?" (Only zeroDigest)
            insertGrant connection collection (contributor (OneArticle collection articleIdentifier))
            other <- requireRight (parseArticleId "123e4567-e89b-42d3-a456-426614174011")
            otherUrl <- requireRight (parseWebUrl "https://example.com/other")
            addArticle connection child (pure instant) (pure other) otherUrl newTitle >>= (@?= Left StoreDenied)
            identifiers <- map (articleId . storedArticle) <$> storedArticles connection
            identifiers @?= [articleIdentifier, newIdentifier]
        , testCase "a stored URL with another title leaves one unchanged article and the same result" . withAddDatabase $ \connection owner -> do
            insertArticle connection articleFixture{storedRevision = 4}
            before <- storedArticles connection
            existingUrl <- requireRight (parseWebUrl "https://example.com/")
            addArticle connection owner (pure instant) (pure newIdentifier) existingUrl newTitle >>= (@?= Right ())
            storedArticles connection >>= assertSame before
        , testCase "a primary-key collision is a fault and changes nothing" . withAddDatabase $ \connection owner -> do
            insertArticle connection articleFixture
            before <- storedArticles connection
            addArticle connection owner (pure instant) (pure articleIdentifier) newUrl newTitle >>= (@?= Left StoreFault)
            storedArticles connection >>= assertSame before
            assertAutocommit connection
        , testCase "authorization and the generator run inside the write transaction" . withAddDatabase $ \connection owner -> do
            samples <- newIORef []
            let inside label = do
                    autocommit <- Direct.getAutoCommit (connectionHandle connection)
                    modifyIORef' samples ((label, autocommit) :)
                clock = inside "clock" >> pure instant
                generate = inside "generator" >> pure newIdentifier
            addArticle connection owner clock generate newUrl newTitle >>= (@?= Right ())
            readIORef samples >>= (@?= [("generator", False), ("clock", False)])
        , testCase "an exception from the generator rolls the addition back" . withAddDatabase $ \connection owner -> do
            result <- try @ErrorCall (addArticle connection owner (pure instant) (throwIO (ErrorCall "generator failed")) newUrl newTitle)
            either (const (pure ())) (const (assertFailure "the exception was not propagated")) result
            storedArticles connection >>= assertSame []
            assertAutocommit connection
        , testCase "cancellation inside the transaction rolls the addition back" . withAddDatabase $ \connection owner -> do
            entered <- newEmptyMVar
            release <- newEmptyMVar
            finished <- newEmptyMVar
            let generate = putMVar entered () >> takeMVar release >> pure newIdentifier
            worker <- forkFinally (addArticle connection owner (pure instant) generate newUrl newTitle) (putMVar finished)

            await (takeMVar entered)
            killThread worker
            outcome <- await (takeMVar finished)
            putMVar release ()

            either (const (pure ())) (const (assertFailure "the addition was not cancelled")) outcome
            storedArticles connection >>= assertSame []
            assertAutocommit connection
        ]
  where
    contributor scope =
        StoredGrant
            { storedGrant = GrantSnapshot childIdentifier (Just rootIdentifier) scope (Set.singleton AddArticles) (Just deadline) 1
            , storedTokenHash = zeroDigest
            , storedPetname = Nothing
            }

newIdentifier :: ArticleId
newIdentifier = either (error . show) id (parseArticleId "123e4567-e89b-42d3-a456-426614174010")

newUrl :: WebUrl
newUrl = either (error . show) id (parseWebUrl "https://example.com/new")

newTitle :: Title
newTitle = either (error . show) id (mkTitle "A new article")

instant, deadline :: UTCTime
instant = fromTimestamp 1000000
deadline = fromTimestamp 2000000

withAddDatabase :: (Connection -> Bearer -> IO a) -> IO a
withAddDatabase use = withEmptyDatabase $ \_ _ connection -> do
    requireRight =<< initializeSchema connection collection
    insertGrant connection collection rootFixture{storedTokenHash = goldenDigest}
    owner <- bearerFor goldenToken
    use connection owner

bearerFor :: Text -> IO Bearer
bearerFor = requireRight . parseBearer

storedArticles :: Connection -> IO [StoredArticle]
storedArticles connection = do
    rows <- SQL.query_ connection "SELECT id, title, url, state, created_at, revision FROM article ORDER BY id"
    traverse (requireRight . fromArticleRow) rows

assertAutocommit :: Connection -> Assertion
assertAutocommit connection = Direct.getAutoCommit (connectionHandle connection) >>= (@?= True)
