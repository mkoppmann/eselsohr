{- |
Module      : BearerPageTests
Description : Tests of the bearer page through the whole application

Each test creates real collections in a temporary directory, adds grants and articles through SQLite, and requests pages from the application in process, so the bearer is parsed, bound, and checked by the protected operations as in production.
One test serves the owner page over a real listener to check its headers and HEAD.
The tests cover the owner, a single-article reader, an add-only contributor, a delete-only holder, two collections that share an article identifier, invalid locators, escaping, and the responses to a busy or corrupt collection.
-}
module BearerPageTests (bearerPageTests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..), fromGregorian)
import Eselsohr.Capability (Permission (..), Scope (..))
import Eselsohr.Config (Listener (..))
import Eselsohr.Id (renderCollectionId)
import Eselsohr.Store.SQLite.Bearer (newBearer, renderBearer)
import Eselsohr.Web.Server (application, securityHeaders, serverSettings)
import Network.Wai.Handler.Warp (testWithApplicationSettings)
import RawHttp (RawResponse (..), exchange, header, request)
import System.Directory (listDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import WebFixtures

bearerPageTests :: TestTree
bearerPageTests =
    testGroup
        "Bearer page"
        [ testCase "the owner sees every article newest first and the add form" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            _ <- addArticle owner articleA "Older article" "https://example.com/older" (day 1)
            _ <- addArticle owner articleB "Newer article" "https://example.com/newer" (day 2)

            page <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken owner))
            exchangeStatus page @?= 200
            assertOrder (exchangeBody page) "Newer article" "Older article"
            assertContains (exchangeBody page) ("action=\"/b/" <> encodeUtf8 (ownerToken owner) <> "/add\"")
        , testCase "a real listener serves the owner page with every header, and HEAD without a body" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            _ <- addArticle owner articleA "Listed article" "https://example.com/listed" (day 1)
            let path = "/b/" <> encodeUtf8 (ownerToken owner)

            testWithApplicationSettings (serverSettings (Listener "127.0.0.1" 0) ignoreMessages) (pure (application ignoreMessages directory)) $ \port -> do
                page <- exchange (fromIntegral port) (request "GET" path [] "")
                head' <- exchange (fromIntegral port) (request "HEAD" path [] "")
                rawStatus page @?= 200
                assertContains (rawBody page) "Listed article"
                forM_ securityHeaders $ \(name, value) -> header name page @?= [value]
                header "Set-Cookie" page @?= []
                (rawStatus head', rawBody head') @?= (200, "")
                header "Content-Length" head' @?= header "Content-Length" page
        , testCase "a single-article reader sees only its article" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            first' <- addArticle owner articleA "Shared article" "https://example.com/shared" (day 1)
            _ <- addArticle owner articleB "Private article" "https://example.com/private" (day 2)
            reader <- addGrant owner 1 (Set.singleton ReadArticles) (OneArticle (collectionIdentifier owner) first') later

            page <- runApplication (application ignoreMessages directory) (pageRequest reader)
            exchangeStatus page @?= 200
            assertContains (exchangeBody page) "Shared article"
            assertMissing (exchangeBody page) "Private article"
            assertMissing (exchangeBody page) "<form"
        , testCase "an add-only contributor sees the add form and no article" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            _ <- addArticle owner articleA "Hidden article" "https://example.com/hidden" (day 1)
            contributor <- addGrant owner 2 (Set.singleton AddArticles) (WholeCollection (collectionIdentifier owner)) later

            page <- runApplication (application ignoreMessages directory) (pageRequest contributor)
            exchangeStatus page @?= 200
            assertContains (exchangeBody page) ("action=\"/b/" <> encodeUtf8 contributor <> "/add\"")
            assertMissing (exchangeBody page) "Hidden article"
            assertMissing (exchangeBody page) "example.com"
        , testCase "a delete-only holder of one article sees no contents" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            target <- addArticle owner articleA "Target article" "https://example.com/target" (day 1)
            holder <- addGrant owner 3 (Set.singleton DeleteArticles) (OneArticle (collectionIdentifier owner) target) later

            page <- runApplication (application ignoreMessages directory) (pageRequest holder)
            exchangeStatus page @?= 200
            assertMissing (exchangeBody page) "Target article"
            assertMissing (exchangeBody page) "example.com"
            assertContains (exchangeBody page) "This link offers no actions on this page."
        , testCase "two collections with the same article identifier show their own articles" . withCollections $ \_ directory -> do
            first' <- newCollection directory
            second' <- newCollection directory
            _ <- addArticle first' articleA "First collection" "https://example.com/first" (day 1)
            _ <- addArticle second' articleA "Second collection" "https://example.com/second" (day 1)

            firstPage <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken first'))
            secondPage <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken second'))
            assertContains (exchangeBody firstPage) "First collection"
            assertMissing (exchangeBody firstPage) "Second collection"
            assertContains (exchangeBody secondPage) "Second collection"
            assertMissing (exchangeBody secondPage) "First collection"
        , testCase "titles and URLs are escaped" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            _ <- addArticle owner articleA "<script>alert(\"x\")</script> & 'quotes'" "https://example.com/?a=1&b=2" (day 1)

            page <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken owner))
            assertMissing (exchangeBody page) "<script"
            assertContains (exchangeBody page) "&lt;script&gt;"
            assertContains (exchangeBody page) "href=\"https://example.com/?a=1&amp;b=2\""
            assertMissing (exchangeBody page) "a=1&b=2"
        , testCase "malformed, oversized, unknown, expired, and revoked bearers get the same response" . withCollections $ \path directory -> do
            owner <- newCollection directory
            expired <- addGrant owner 4 (Set.singleton ReadArticles) (WholeCollection (collectionIdentifier owner)) (day 1)
            revoked <- addGrant owner 5 (Set.singleton ReadArticles) (WholeCollection (collectionIdentifier owner)) later
            removeGrant owner revoked
            unknownSecret <- either (assertFailure . show) (pure . renderBearer) (newBearer (collectionIdentifier owner) (ByteString.replicate 32 0xee))
            before <- listDirectory path

            reference <- runApplication (application ignoreMessages directory) (pageRequest "v2.malformed")
            exchangeStatus reference @?= 404
            forM_ [Text.replicate 1000 "A", unknownCollection, unknownSecret, expired, revoked] $ \token -> do
                page <- runApplication (application ignoreMessages directory) (pageRequest token)
                (exchangeStatus page, exchangeBody page) @?= (exchangeStatus reference, exchangeBody reference)
            listDirectory path >>= (@?= before)
        , testCase "a busy collection gives 503 with Retry-After and no operator message" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            (logMessage, logged) <- capturingMessages

            page <- holdExclusive owner (runApplication (application logMessage directory) (pageRequest (ownerToken owner)))
            exchangeStatus page @?= 503
            lookup "Retry-After" (exchangeHeaders page) @?= Just "1"
            logged >>= (@?= [])
        , testCase "a corrupt collection gives 500 and an operator message naming the collection" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            corruptArticle owner articleA (day 1)
            (logMessage, logged) <- capturingMessages

            page <- runApplication (application logMessage directory) (pageRequest (ownerToken owner))
            exchangeStatus page @?= 500
            messages <- logged
            messages @?= ["Collection " <> renderCollectionId (collectionIdentifier owner) <> " has corrupt data."]
            assertBool "the bearer reached a message" (not (any (Text.isInfixOf (Text.takeWhileEnd (/= '.') (ownerToken owner))) messages))
        ]
  where
    articleA = "123e4567-e89b-42d3-a456-426614174002"
    articleB = "123e4567-e89b-42d3-a456-426614174003"
    unknownCollection = "v2.Ej5FZ-ibQtOkVkJmFBdAAA.AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8"

day :: Integer -> UTCTime
day n = UTCTime (fromGregorian 2026 1 1) 0 `addDays'` n
  where
    addDays' (UTCTime date time) days = UTCTime (toEnum (fromEnum date + fromInteger days)) time

-- | A deadline far in the future of the host clock that binds the bearer.
later :: UTCTime
later = UTCTime (fromGregorian 2100 1 1) 0

assertContains :: ByteString -> ByteString -> Assertion
assertContains html needle = assertBool ("missing " <> Char8.unpack needle) (needle `ByteString.isInfixOf` html)

assertMissing :: ByteString -> ByteString -> Assertion
assertMissing html needle = assertBool ("unexpected " <> Char8.unpack needle) . not $ needle `ByteString.isInfixOf` html

assertOrder :: ByteString -> ByteString -> ByteString -> Assertion
assertOrder html first' second' =
    assertBool "articles are not newest first" $
        ByteString.length (fst (ByteString.breakSubstring first' html)) < ByteString.length (fst (ByteString.breakSubstring second' html))
