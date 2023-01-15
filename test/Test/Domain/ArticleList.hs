module Test.Domain.ArticleList
    ( articleListSpec
    ) where

import Data.Map.Strict qualified as Map

import Data.Map.Strict ((!))
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotSatisfy
    )

import Lib.Domain.Article qualified as Article
import Lib.Domain.ArticleList qualified as ArticleList
import Lib.Domain.Authorization qualified as Authz

import Lib.Domain.Article (Article)
import Lib.Domain.ArticleList (ArticleList)
import Lib.Domain.Error (notFound)
import Lib.Domain.Id (Id)
import Test.Domain.Shared
    ( authorized
    , defaultArticle
    , getNonEmptyText
    , getRandomId
    , objRefWithAllArticlesPerms
    )

articleListSpec :: Spec
articleListSpec = describe "Lib.Domain.ArticleList" $ do
    it "can add a new article" $ do
        article <- defaultArticle
        let articleId = article.id
            articleList = ArticleList.mkArticleList
            perm = authorized $ Authz.canCreateArticles objRefWithAllArticlesPerms
            newArticleList = case ArticleList.addArticle perm articleId article articleList of
                Left _err -> articleList
                Right newList -> newList
            addedArticle = getArticleWithId articleId newArticleList
        addedArticle `shouldBe` article

    it "cannot add an existing article" $ do
        article <- defaultArticle
        let articleId = article.id
            articleList = defaultArticleListWithArticle article
            perm = authorized $ Authz.canCreateArticles objRefWithAllArticlesPerms
            err = ArticleList.addArticle perm articleId article articleList
        err `shouldBe` Left ArticleList.articleAlreadyExists

    it "can change the title of an existing article" $ do
        article <- defaultArticle
        let articleId = article.id
            articleList = defaultArticleListWithArticle article
            newTitle = getNonEmptyText "New title"
            perm = authorized $ Authz.canChangeArticleTitle objRefWithAllArticlesPerms articleId
            newArticleList = case ArticleList.changeArticleTitle perm newTitle articleList of
                Left _err -> articleList
                Right newList -> newList
            changedArticle = getArticleWithId articleId newArticleList
        changedArticle.title `shouldBe` newTitle

    it "returns an error when changing the title of a non-existing article" $ do
        articleId <- getRandomId
        let articleList = ArticleList.mkArticleList
            newTitle = getNonEmptyText "New title"
            perm = authorized $ Authz.canChangeArticleTitle objRefWithAllArticlesPerms articleId
            err = ArticleList.changeArticleTitle perm newTitle articleList
        err `shouldBe` Left notFound

    it "can mark an existing article as read" $ do
        article <- defaultArticle
        let articleId = article.id
            articleList = defaultArticleListWithArticle article
            perm = authorized $ Authz.canChangeArticleState objRefWithAllArticlesPerms articleId
            newArticleList = case ArticleList.markArticleAsRead perm articleList of
                Left _err -> articleList
                Right newList -> newList
            readArticle = getArticleWithId articleId newArticleList
        readArticle.state `shouldBe` Article.Read

    it "returns an error when marking a non-existing article as read" $ do
        articleId <- getRandomId
        let articleList = ArticleList.mkArticleList
            perm = authorized $ Authz.canChangeArticleState objRefWithAllArticlesPerms articleId
            err = ArticleList.markArticleAsRead perm articleList
        err `shouldBe` Left notFound

    it "can mark an existing article as unread" $ do
        article <- defaultArticle
        let articleId = article.id
            articleList = defaultArticleListWithArticle article
            perm = authorized $ Authz.canChangeArticleState objRefWithAllArticlesPerms articleId
            newArticleList = case ArticleList.markArticleAsUnread perm articleList of
                Left _err -> articleList
                Right newList -> newList
            unreadArticle = getArticleWithId articleId newArticleList
        unreadArticle.state `shouldBe` Article.Unread

    it "returns an error when marking a non-existing article as unread" $ do
        articleId <- getRandomId
        let articleList = ArticleList.mkArticleList
            perm = authorized $ Authz.canChangeArticleState objRefWithAllArticlesPerms articleId
            err = ArticleList.markArticleAsUnread perm articleList
        err `shouldBe` Left notFound

    it "can delete an existing article" $ do
        article <- defaultArticle
        let articleId = article.id
            articleList = defaultArticleListWithArticle article
            perm = authorized $ Authz.canDeleteArticle objRefWithAllArticlesPerms articleId
            newArticleList = ArticleList.removeArticle perm articleList
        newArticleList `shouldNotSatisfy` articleInList articleId

    it "can delete a non-existing article" $ do
        articleId <- getRandomId
        let articleList = ArticleList.mkArticleList
            perm = authorized $ Authz.canDeleteArticle objRefWithAllArticlesPerms articleId
            newArticleList = ArticleList.removeArticle perm articleList
        newArticleList `shouldNotSatisfy` articleInList articleId

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

defaultArticleListWithArticle :: Article -> ArticleList
defaultArticleListWithArticle art = ArticleList.fromMap $ Map.insert art.id art Map.empty

getArticleWithId :: Id Article -> ArticleList -> Article
getArticleWithId artId artList = ArticleList.toMap artList ! artId

articleInList :: Id Article -> ArticleList -> Bool
articleInList artId = Map.member artId . ArticleList.toMap
