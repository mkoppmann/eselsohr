module Lib.Ui.Web.Controller.ArticleList
  ( articleList
  ) where

import qualified Lib.App.Command                                     as Command
import qualified Lib.App.Env                                         as Env
import qualified Lib.Domain.Uri                                      as Uri
import qualified Lib.Ui.Web.Page.Article                             as ArticlePage
import qualified Lib.Ui.Web.Page.ArticleList                         as ArticleListPage
import qualified Lib.Ui.Web.Page.CreateArticle                       as CreateArticlePage
import qualified Lib.Ui.Web.Page.EditArticle                         as EditArticlePage
import qualified Lib.Ui.Web.Page.ShareArticle                        as ShareArticlePage
import qualified Lib.Ui.Web.Page.ShareArticleList                    as ShareArticleListPage
import qualified Lib.Ui.Web.Route                                    as Route

import           Lib.App.Env                                          ( DeploymentMode
                                                                      , Has
                                                                      , grab
                                                                      )
import           Lib.App.Port                                         ( MonadScraper )
import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.Capability                                ( Capability
                                                                      , mkArticlePerms
                                                                      , mkArticlesPerms
                                                                      )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.ArticleList                          ( ArticleListRepo )
import           Lib.Domain.Repo.CapabilityList                       ( CapabilityListRepo )
import           Lib.Infra.Error                                      ( redirectTo
                                                                      , throwOnError
                                                                      , throwOnErrorM
                                                                      )
import           Lib.Ui.Dto.Accesstoken                               ( collectionId )
import           Lib.Ui.Web.Dto.ExpirationDate                        ( ExpirationDate(..) )
import           Lib.Ui.Web.Dto.Form                                  ( ChangeArticleStateForm(..)
                                                                      , ChangeArticleTitleForm(..)
                                                                      , CreateArticleForm(..)
                                                                      , CreateSharedArticleListRefForm(..)
                                                                      , CreateSharedArticleRefForm(..)
                                                                      , DeleteItemForm(..)
                                                                      )
import           Lib.Ui.Web.Page.Shared                               ( WithQuery
                                                                      , lookupReferences
                                                                      )
import           Lib.Ui.Web.Route                                     ( AppServer
                                                                      , ArticleListSite
                                                                      , Redirection
                                                                      )

articleList :: ArticleListSite AppServer
articleList = Route.ArticleListSite { Route.articleListPage            = ArticleListPage.handler
                                    , Route.createArticlePage          = CreateArticlePage.handler
                                    , Route.createArticle              = createArticle
                                    , Route.articlePage                = ArticlePage.handler
                                    , Route.editArticlePage            = EditArticlePage.handler
                                    , Route.changeArticleTitle         = changeArticleTitle
                                    , Route.markArticleAsRead          = markArticleAsRead
                                    , Route.markArticleAsUnread        = markArticleAsUnread
                                    , Route.deleteArticle              = deleteArticle
                                    , Route.shareArticleListPage       = ShareArticleListPage.handler
                                    , Route.createSharedArticleListRef = createSharedArticleListRef
                                    , Route.deleteSharedArticleListRef = deleteSharedArticleListRef
                                    , Route.shareArticlePage           = ShareArticlePage.handler
                                    , Route.createSharedArticleRef     = createSharedArticleRef
                                    , Route.deleteSharedArticleRef     = deleteSharedArticleRef
                                    }

type WithEnv env m = (MonadReader env m, Has DeploymentMode env)

createArticle
  :: (ArticleListRepo m, MonadScraper m, WithEnv env m, WithQuery env m) => CreateArticleForm -> m Redirection
createArticle CreateArticleForm {..} = do
  (ref, objRef)  <- lookupReferences acc
  deploymentMode <- grab @DeploymentMode
  command        <- throwOnError . mkCommand deploymentMode objRef $ collectionId ref
  throwOnErrorM $ Command.createArticle command
  redirectTo goto
 where
  mkCommand deploymentMode objRef colId = do
    let mkUri = case deploymentMode of
          Env.Prod -> Uri.mkUri
          Env.Test -> Uri.unfilteredUri
          Env.Dev  -> Uri.unfilteredUri
    uri <- mkUri articleUri
    Right Command.CreateArticle { .. }

changeArticleTitle :: (ArticleListRepo m, WithQuery env m) => Id Article -> ChangeArticleTitleForm -> m Redirection
changeArticleTitle artId ChangeArticleTitleForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.changeArticleTitle command
  redirectTo goto
 where
  mkCommand objRef colId = do
    let title = articleTitle
    Command.ChangeArticleTitle { .. }

markArticleAsRead :: (ArticleListRepo m, WithQuery env m) => Id Article -> ChangeArticleStateForm -> m Redirection
markArticleAsRead artId ChangeArticleStateForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.markArticleAsRead command
  redirectTo goto
  where mkCommand objRef colId = Command.MarkArticleAsRead { .. }

markArticleAsUnread :: (ArticleListRepo m, WithQuery env m) => Id Article -> ChangeArticleStateForm -> m Redirection
markArticleAsUnread artId ChangeArticleStateForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.markArticleAsUnread command
  redirectTo goto
  where mkCommand objRef colId = Command.MarkArticleAsUnread { .. }

deleteArticle :: (ArticleListRepo m, WithQuery env m) => Id Article -> DeleteItemForm -> m Redirection
deleteArticle artId DeleteItemForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.deleteArticle command
  redirectTo goto
  where mkCommand objRef colId = Command.DeleteArticle { .. }

createSharedArticleListRef
  :: (CapabilityListRepo m, WithQuery env m) => CreateSharedArticleListRefForm -> m Redirection
createSharedArticleListRef CreateSharedArticleListRefForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.addShareArticleList command
  redirectTo goto
 where
  mkCommand objRef colId = do
    let mPetname    = petname
        mExpDate    = unExpirationDate <$> expirationDate
        share       = False
        sharedPerms = mkArticlesPerms (isJust viewArticles)
                                      (isJust createArticles)
                                      (isJust changeTitle)
                                      (isJust changeState)
                                      (isJust delete)
                                      share
    Command.AddShareArticleList { .. }

deleteSharedArticleListRef
  :: (CapabilityListRepo m, WithQuery env m) => Id Capability -> DeleteItemForm -> m Redirection
deleteSharedArticleListRef capId DeleteItemForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.deleteShareArticleList command
  redirectTo goto
  where mkCommand objRef colId = Command.DeleteShareArticleList { .. }

createSharedArticleRef
  :: (CapabilityListRepo m, WithQuery env m) => Id Article -> CreateSharedArticleRefForm -> m Redirection
createSharedArticleRef artId CreateSharedArticleRefForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.addShareArticle command
  redirectTo goto
 where
  mkCommand objRef colId = do
    let
      mPetname = petname
      mExpDate = unExpirationDate <$> expirationDate
      share    = False
      sharedPerms =
        mkArticlePerms artId (isJust viewArticle) (isJust changeTitle) (isJust changeState) (isJust delete) share
    Command.AddShareArticle { .. }

deleteSharedArticleRef
  :: (CapabilityListRepo m, WithQuery env m) => Id Article -> Id Capability -> DeleteItemForm -> m Redirection
deleteSharedArticleRef artId capId DeleteItemForm {..} = do
  (ref, objRef) <- lookupReferences acc
  let command = mkCommand objRef $ collectionId ref
  throwOnErrorM $ Command.deleteShareArticle command
  redirectTo goto
  where mkCommand objRef colId = Command.DeleteShareArticle { .. }
