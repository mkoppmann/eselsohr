module Test.Domain.Article
    ( articleSpec
    ) where

import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

import Lib.Domain.Article qualified as Article

import Test.Domain.Shared
    ( defaultArticle
    , defaultArticleWithState
    , getNonEmptyText
    )

articleSpec :: Spec
articleSpec = describe "Lib.Domain.Article" $ do
    it "articles with same IDs but different values are equal" $ do
        article1 <- defaultArticleWithState Article.Unread
        let article2 = article1{Article.title = getNonEmptyText "New title", Article.state = Article.Read}
        article1 `shouldSatisfy` (== article2)

    it "articles with different IDs but same values are not equal" $ do
        article1 <- defaultArticle
        article2 <- defaultArticle
        article1 `shouldSatisfy` (/= article2)

    it "can change the title of an article" $ do
        article <- defaultArticle
        let newTitle = getNonEmptyText "New title"
            newArticle = Article.changeTitle newTitle article
        newArticle.title `shouldBe` newTitle

    it "a read article can be marked as unread" $ do
        article <- defaultArticleWithState Article.Read
        let unreadArticle = Article.markAsUnread article
        unreadArticle.state `shouldBe` Article.Unread

    it "an unread article stays unread when marked as unread" $ do
        article <- defaultArticleWithState Article.Unread
        let readArticle = Article.markAsUnread article
        readArticle.state `shouldBe` Article.Unread

    it "an unread article can be marked as read" $ do
        article <- defaultArticleWithState Article.Unread
        let readArticle = Article.markAsRead article
        readArticle.state `shouldBe` Article.Read

    it "a read article stays read when marked as read" $ do
        article <- defaultArticleWithState Article.Unread
        let readArticle = Article.markAsRead article
        readArticle.state `shouldBe` Article.Read
