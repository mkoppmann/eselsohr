{- |
Module      : AdditionTests
Description : Tests of the add form through the whole application

Each test submits the add form to the application in process, with real collections in a temporary directory, and checks the redirect, the submission notice, the stored articles, and the 422, 400, and 413 responses.
Receipts for new and stored URLs must be identical, and an add-only holder's receipt page must show no stored article, count, or other bearer URL.
-}
module AdditionTests (additionTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..), fromGregorian)
import Eselsohr.Capability (Permission (..), Scope (..))
import Eselsohr.Store.Runtime (CollectionDirectory)
import Eselsohr.Web.Server (application)
import Network.HTTP.Types (renderSimpleQuery)
import Network.Wai (Request (..), RequestBodyLength (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import WebFixtures

additionTests :: TestTree
additionTests =
    testGroup
        "Add form"
        [ testCase "the owner adds an article and sees it with the notice" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            response <- submit directory (ownerToken owner) [("url", "https://example.com/new"), ("title", "A new article")]
            exchangeStatus response @?= 303
            lookup "Location" (exchangeHeaders response) @?= Just ("/b/" <> encodeUtf8 (ownerToken owner) <> "?added")

            page <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken owner)){queryString = [("added", Nothing)]}
            assertContains (exchangeBody page) "Your article was submitted."
            assertContains (exchangeBody page) "A new article"
            plain <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken owner))
            assertMissing (exchangeBody plain) "Your article was submitted."
        , testCase "receipts for new and stored URLs are identical and reveal nothing to a contributor" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            _ <- addArticle owner "123e4567-e89b-42d3-a456-426614174002" "Stored article" "https://example.com/stored" (day 1)
            contributor <- addGrant owner 2 (Set.singleton AddArticles) (WholeCollection (collectionIdentifier owner)) later

            fresh <- submit directory contributor [("url", "https://example.com/fresh"), ("title", "Fresh")]
            stored <- submit directory contributor [("url", "https://example.com/stored"), ("title", "Another title")]
            (exchangeStatus fresh, exchangeHeaders fresh, exchangeBody fresh) @?= (exchangeStatus stored, exchangeHeaders stored, exchangeBody stored)

            receipt <- runApplication (application ignoreMessages directory) (pageRequest contributor){queryString = [("added", Nothing)]}
            assertContains (exchangeBody receipt) "Your article was submitted."
            mapM_ (assertMissing (exchangeBody receipt)) ["Stored article", "Fresh", "example.com", encodeUtf8 (ownerToken owner)]
            assertSingleBearer (exchangeBody receipt) contributor

            ownerPage <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken owner))
            assertContains (exchangeBody ownerPage) "Stored article"
            assertMissing (exchangeBody ownerPage) "Another title"
            assertContains (exchangeBody ownerPage) "Fresh"
        , testCase "invalid and empty fields give 422 with escaped values and store nothing" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            rejected <- submit directory (ownerToken owner) [("url", "\"><script>alert(1)</script>"), ("title", "")]
            exchangeStatus rejected @?= 422
            assertContains (exchangeBody rejected) "The URL must be"
            assertContains (exchangeBody rejected) "The title must"
            assertContains (exchangeBody rejected) "value=\"&quot;&gt;&lt;script&gt;alert(1)&lt;/script&gt;\""
            assertMissing (exchangeBody rejected) "<script"

            page <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken owner))
            assertContains (exchangeBody page) "No articles yet."
        , testCase "method override and redirect fields are ignored" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            response <- submit directory (ownerToken owner) [("url", "https://example.com/a"), ("title", "A"), ("_method", "DELETE"), ("goto", "https://evil.example/")]
            exchangeStatus response @?= 303
            lookup "Location" (exchangeHeaders response) @?= Just ("/b/" <> encodeUtf8 (ownerToken owner) <> "?added")
        , testCase "a one-article holder's submission is denied, valid or not" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            target <- addArticle owner "123e4567-e89b-42d3-a456-426614174002" "Target" "https://example.com/target" (day 1)
            holder <- addGrant owner 3 (Set.fromList [ReadArticles, AddArticles]) (OneArticle (collectionIdentifier owner) target) later
            invalidLink <- runApplication (application ignoreMessages directory) (pageRequest "v2.malformed")

            valid <- submit directory holder [("url", "https://example.com/other"), ("title", "Other")]
            invalid <- submit directory holder [("url", ""), ("title", "")]
            mapM_ (\response -> (exchangeStatus response, exchangeBody response) @?= (404, exchangeBody invalidLink)) [valid, invalid]

            page <- runApplication (application ignoreMessages directory) (pageRequest (ownerToken owner))
            assertMissing (exchangeBody page) "Other"
        , testCase "an undecodable body gives 400 and an oversized one 413" . withCollections $ \_ directory -> do
            owner <- newCollection directory
            undecodable <- formRequest (ownerToken owner) "add" "url=%FF&title=x"
            runApplication (application ignoreMessages directory) undecodable >>= (@?= 400) . exchangeStatus
            oversized <- formRequest (ownerToken owner) "add" (ByteString.replicate (160 * 1024 + 1) 0x61)
            runApplication (application ignoreMessages directory) oversized{requestBodyLength = ChunkedBody} >>= (@?= 413) . exchangeStatus
        ]

submit :: CollectionDirectory -> Text -> [(ByteString, ByteString)] -> IO Exchange
submit directory token fields = do
    request <- formRequest token "add" (renderSimpleQuery False fields)
    runApplication (application ignoreMessages directory) request

day :: Integer -> UTCTime
day n = UTCTime (toEnum (fromEnum (fromGregorian 2026 1 1) + fromInteger n)) 0

later :: UTCTime
later = UTCTime (fromGregorian 2100 1 1) 0

-- | The page contains no bearer path other than the given bearer's own.
assertSingleBearer :: ByteString -> Text -> Assertion
assertSingleBearer html token =
    assertBool "another bearer URL appears" $
        all (ByteString.isPrefixOf (encodeUtf8 token)) (drop 1 (splitOn "/b/" html))
  where
    splitOn separator text = case ByteString.breakSubstring separator text of
        (before, rest)
            | ByteString.null rest -> [before]
            | otherwise -> before : splitOn separator (ByteString.drop (ByteString.length separator) rest)

assertContains :: ByteString -> ByteString -> Assertion
assertContains html needle = assertBool ("missing " <> Char8.unpack needle) (needle `ByteString.isInfixOf` html)

assertMissing :: ByteString -> ByteString -> Assertion
assertMissing html needle = assertBool ("unexpected " <> Char8.unpack needle) . not $ needle `ByteString.isInfixOf` html
