{- |
Module      : PageTests
Description : Tests of the pure bearer page

The tests render the page for grant descriptions directly and check which sections appear: the add form only for whole-collection AddArticles, articles only with ReadArticles, a message for empty collections, the neutral text when nothing is permitted, and the deadline of an expiring grant.
They also check the submission notice, the submitted values and field messages of a failed submission, and the decoding and validation of the add form.
-}
module PageTests (pageTests) where

import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Eselsohr.Capability (Permission (..), Scope (..))
import Eselsohr.Id (CollectionId, parseArticleId, parseCollectionId)
import Eselsohr.Store (GrantView (..))
import Eselsohr.Web.Form (AddInput (..), FieldProblem (..), decodeAddForm, validateAdd)
import Eselsohr.Web.Page (PageExtras (..), plainPage, renderBearerPage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

pageTests :: TestTree
pageTests =
    testGroup
        "Bearer page sections"
        [ testCase "whole-collection AddArticles shows the add form" $ do
            let html = render (GrantView (Set.singleton AddArticles) whole Nothing) Nothing
            assertContains html "action=\"/b/token/add\""
            assertMissing html "Articles</h2>"
        , testCase "AddArticles on one article shows no add form" $ do
            let html = render (GrantView (Set.fromList [AddArticles, DeleteArticles]) oneArticle Nothing) Nothing
            assertMissing html "<form"
            assertContains html "This link offers no actions on this page."
        , testCase "an empty readable collection says so" $ do
            let html = render (GrantView (Set.singleton ReadArticles) whole Nothing) (Just [])
            assertContains html "No articles yet."
            assertMissing html "<form"
            assertMissing html "This link offers no actions"
        , testCase "a grant without permissions shows the neutral text" $
            assertContains (render (GrantView Set.empty whole Nothing) Nothing) "This link offers no actions on this page."
        , testCase "the submission notice and a failed submission's values and messages" $ do
            let adder = GrantView (Set.singleton AddArticles) whole Nothing
                input = AddInput "\"><script>" "<b>title</b>" [InvalidUrl, InvalidTitle]
                notice = LazyByteString.toStrict (renderBearerPage "token" plainPage{extrasSubmitted = True} adder Nothing)
                rejected = LazyByteString.toStrict (renderBearerPage "token" plainPage{extrasInput = input} adder Nothing)
            assertContains notice "Your article was submitted."
            assertMissing rejected "Your article was submitted."
            assertContains rejected "The URL must be"
            assertContains rejected "The title must"
            assertContains rejected "value=\"&quot;&gt;&lt;script&gt;\""
            assertContains rejected "value=\"&lt;b&gt;title&lt;/b&gt;\""
            assertMissing rejected "<script"
        , testCase "form decoding ignores other fields and rejects non-UTF-8 text" $ do
            decodeAddForm "url=https%3A%2F%2Fexample.com%2F&title=A+title&_method=DELETE&goto=https%3A%2F%2Fevil.example" @?= Just ("https://example.com/", "A title")
            decodeAddForm "title=Only" @?= Just ("", "Only")
            decodeAddForm "url=%FF&title=x" @?= Nothing
        , testCase "validation reports every failing field and trims the URL" $ do
            void (validateAdd " https://example.com/ " "A title") @?= Right ()
            void (validateAdd "" "") @?= Left [InvalidUrl, InvalidTitle]
            void (validateAdd "ftp://example.com/" "A title") @?= Left [InvalidUrl]
            void (validateAdd "https://example.com/" (Text.replicate 501 "a")) @?= Left [InvalidTitle]
        , testCase "an expiring grant shows its deadline" $
            assertContains
                (render (GrantView (Set.singleton ReadArticles) whole (Just (UTCTime (fromGregorian 2026 10 5) 3600))) (Just []))
                "This link expires on 2026-10-05 at 01:00 UTC."
        ]
  where
    render view = LazyByteString.toStrict . renderBearerPage "token" plainPage view
    whole = WholeCollection collection
    oneArticle = OneArticle collection (either (error . show) id (parseArticleId "123e4567-e89b-42d3-a456-426614174002"))

collection :: CollectionId
collection = either (error . show) id (parseCollectionId "123e4567-e89b-42d3-a456-426614174000")

assertContains :: ByteString.ByteString -> String -> Assertion
assertContains html needle = assertBool ("missing " <> needle) (Char8.pack needle `ByteString.isInfixOf` html)

assertMissing :: ByteString.ByteString -> String -> Assertion
assertMissing html needle = assertBool ("unexpected " <> needle) . not $ Char8.pack needle `ByteString.isInfixOf` html
