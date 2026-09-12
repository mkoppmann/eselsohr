{- |
Module      : Articles
Description : Article construction and transition tests

'articleTests' checks that construction preserves supplied values and starts articles unread, and that equality compares every field.
Examples and generated articles check that title and read-state edits preserve unrelated fields and are idempotent.
-}
module Articles (articleTests) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day (..), UTCTime (..), addUTCTime, picosecondsToDiffTime)
import Eselsohr.Article
import Eselsohr.Id (ArticleId, parseArticleId)
import Eselsohr.WebUrl (parseWebUrl, renderWebUrl)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
    ( Property
    , QuickCheckTests (..)
    , chooseInt
    , counterexample
    , elements
    , forAll
    , testProperty
    , (===)
    )

articleTests :: TestTree
articleTests = localOption (QuickCheckTests 500) $ testGroup "Article" [snapshotTests, transitionTests]

------------------------------------------------------------------------
-- Construction and equality
------------------------------------------------------------------------

snapshotTests :: TestTree
snapshotTests =
    testGroup
        "Construction and equality"
        [ testProperty "construction preserves inputs and starts unread" . withArticle $ \article ->
            let created = newArticle (articleId article) (articleTitle article) (articleUrl article) (articleCreatedAt article)
             in fields created === (articleId article, articleTitle article, renderWebUrl (articleUrl article), Unread, articleCreatedAt article)
        , testCase "equality compares every field, including contents with the same ID" $ do
            article <- fixture
            other <- rightOrFail (snapshot 8 Read)
            let variants =
                    [ ("ID", article{articleId = articleId other})
                    , ("title", article{articleTitle = articleTitle other})
                    , ("URL", article{articleUrl = articleUrl other})
                    , ("state", article{articleState = articleState other})
                    , ("creation time", article{articleCreatedAt = articleCreatedAt other})
                    ]

            assertBool "identical fields must compare equal" (article == newArticle (articleId article) (articleTitle article) (articleUrl article) (articleCreatedAt article))
            mapM_ (\(name, different) -> assertBool ("equality ignored " ++ name) (article /= different)) variants
        ]

------------------------------------------------------------------------
-- Transitions
------------------------------------------------------------------------

transitionTests :: TestTree
transitionTests =
    testGroup
        "Transitions"
        [ testCase "rename preserves other fields" $ do
            article <- fixture
            title <- rightOrFail (mkTitle "New title")
            fields (rename title article) @?= (articleId article, title, renderWebUrl (articleUrl article), Unread, articleCreatedAt article)
        , testCase "read state changes in both directions" $ do
            article <- fixture
            let readArticle = setReadState Read article
                unreadArticle = setReadState Unread readArticle

            fields readArticle @?= (articleId article, articleTitle article, renderWebUrl (articleUrl article), Read, articleCreatedAt article)
            fields unreadArticle @?= fields article
        , testCase "normalized title is a no-op" $ do
            article <- fixture
            title <- rightOrFail (mkTitle " \tArticle\n7  ")
            fields (rename title article) @?= fields article
        , testProperty "assigning existing values preserves every article field" . withArticle $ \article ->
            (fields (rename (articleTitle article) article), fields (setReadState (articleState article) article)) === (fields article, fields article)
        , testProperty "renaming preserves unrelated fields and is idempotent" . withArticle $ \article ->
            case mkTitle (renderTitle (articleTitle article) <> " edited") of
                Left err -> counterexample (show err) False
                Right title ->
                    let changed = rename title article
                        expected = (articleId article, title, renderWebUrl (articleUrl article), articleState article, articleCreatedAt article)
                     in (fields changed, fields (rename title changed)) === (expected, expected)
        , testProperty "changing read state preserves unrelated fields and is idempotent" . withArticle $ \article ->
            let state = otherState (articleState article)
                changed = setReadState state article
                expected = (articleId article, articleTitle article, renderWebUrl (articleUrl article), state, articleCreatedAt article)
             in (fields changed, fields (setReadState state changed)) === (expected, expected)
        , testProperty "toggling twice restores the original article" . withArticle $ \article ->
            let changed = setReadState (otherState (articleState article)) article
             in fields (setReadState (otherState (articleState changed)) changed) === fields article
        ]

------------------------------------------------------------------------
-- Fixtures and assertions
------------------------------------------------------------------------

otherState :: ArticleState -> ArticleState
otherState Unread = Read
otherState Read = Unread

rightOrFail :: (Show e) => Either e a -> IO a
rightOrFail = either (fail . show) pure

fixture :: IO Article
fixture = rightOrFail (snapshot 7 Unread)

-- Render URLs explicitly in assertions without adding Show to Article.
fields :: Article -> (ArticleId, Title, Text, ArticleState, UTCTime)
fields article = (articleId article, articleTitle article, renderWebUrl (articleUrl article), articleState article, articleCreatedAt article)

withArticle :: (Article -> Property) -> Property
withArticle check = forAll inputs $ \(seed, state) ->
    either (`counterexample` False) check (snapshot seed state)
  where
    inputs = (,) <$> chooseInt (0, 1000000) <*> elements [Unread, Read]

snapshot :: Int -> ArticleState -> Either String Article
snapshot seed state = do
    identifier <- input . parseArticleId $ "123e4567-e89b-42d3-a456-" <> Text.justifyRight 12 '0' suffix
    title <- input . mkTitle $ "Article " <> suffix
    url <- input . parseWebUrl $ "https://example.com/" <> suffix
    pure (Article identifier title url state createdAt)
  where
    suffix = Text.pack (show seed)
    createdAt = addUTCTime (fromIntegral seed) (UTCTime (ModifiedJulianDay (60000 + toInteger seed)) (picosecondsToDiffTime 1234567890123))
    input :: (Show e) => Either e a -> Either String a
    input = either (Left . show) Right
