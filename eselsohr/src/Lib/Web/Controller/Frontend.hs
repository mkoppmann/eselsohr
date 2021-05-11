module Lib.Web.Controller.Frontend
  (  -- * API
    frontend

  -- * Handlers
  , startpage
  ) where

import           Clay                           ( Css )
import           Data.Time                      ( UTCTime(..)
                                                , addGregorianMonthsClip
                                                )
import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( Accesstoken
                                                , Article(..)
                                                , ExpirationDate(..)
                                                , Id
                                                )
import qualified Lib.Core.Domain.Capability    as Cap
import qualified Lib.Core.Domain.Entity        as Entity
import           Lib.Core.Effect                ( MonadTime(..)
                                                , ReadState
                                                )
import qualified Lib.Core.Service              as Service
import           Lib.Web.Controller.Util        ( getContextState
                                                , getObjRef
                                                )
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( AppServer
                                                , ArticleListData(..)
                                                , CollectionOverviewData(..)
                                                , HtmlPage
                                                , ViewArticleData(..)
                                                )
import qualified Lib.Web.View.App              as App
import qualified Lib.Web.View.Page             as Page
import           Lib.Web.View.Style             ( appStylesheet )
import           Servant                        ( fieldLink )

frontend :: Route.FrontendSite AppServer
frontend = Route.FrontendSite { Route.startpage          = startpage
                              , Route.invalidToken       = invalidToken
                              , Route.collectionOverview = collectionOverview
                              , Route.viewArticles       = viewArticles
                              , Route.viewArticle        = viewArticle
                              , Route.editArticle        = editArticle
                              , Route.stylesheet         = stylesheet
                              }

startpage :: (Monad m) => m HtmlPage
startpage = pure $ App.render Page.root

notAuthorized :: HtmlPage
notAuthorized = App.render Page.notAuthorized

collectionOverview
  :: (ReadState m, MonadTime m, WithError m) => Maybe Accesstoken -> m HtmlPage
collectionOverview Nothing    = pure notAuthorized
collectionOverview (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.getUnlockLinks objRef of
    Nothing         -> pure notAuthorized
    Just authAction -> do
      unlockLinks <- Service.getUnlockLinks ctxState authAction
      (earliestExpDate, defaultExpDate) <- getExpirationDates
      let canCreateUnlockLinks = Cap.canCreateUnlockLink objRef
          page = Page.collectionOverview $ CollectionOverviewData
            acc
            canCreateUnlockLinks
            earliestExpDate
            defaultExpDate
            unlockLinks
      pure $ App.render page
 where
  getExpirationDates :: (MonadTime m) => m (ExpirationDate, ExpirationDate)
  getExpirationDates = do
    currTime@UTCTime {..} <- getCurrentTime
    let expDate = currTime { utctDay = addGregorianMonthsClip 1 utctDay }
    pure (ExpirationDate currTime, ExpirationDate expDate)

viewArticles
  :: (ReadState m, MonadTime m, WithError m) => Maybe Accesstoken -> m HtmlPage
viewArticles Nothing    = pure notAuthorized
viewArticles (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.viewArticles objRef of
    Nothing         -> pure notAuthorized
    Just authAction -> do
      articles <- Service.getArticles ctxState authAction
      let canCreateArticles = Cap.canCreateArticle objRef
          canChangeTitles   = Cap.canChangeAllArticleTitles objRef
          canChangeStates   = Cap.canChangeAllArticleStates objRef
          canDelete         = Cap.canDeleteAllArticles objRef
          page = Page.articleList $ ArticleListData acc
                                                    canCreateArticles
                                                    canChangeTitles
                                                    canChangeStates
                                                    canDelete
                                                    articles
      pure $ App.render page

viewArticle
  :: (ReadState m, MonadTime m, WithError m)
  => Id Article
  -> Maybe Accesstoken
  -> m HtmlPage
viewArticle _     Nothing    = pure notAuthorized
viewArticle artId (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.viewArticle objRef artId of
    Nothing         -> pure notAuthorized
    Just authAction -> do
      artEnt <- Service.getArticle ctxState authAction
      let canViewArticles = Cap.canViewArticles objRef
          canChangeTitle  = Cap.canChangeArticleTitle objRef artId
          canChangeState  = Cap.canChangeArticleState objRef artId
          canDelete       = Cap.canDeleteArticle objRef artId
          page            = Page.showArticle $ ViewArticleData acc
                                                               canViewArticles
                                                               canChangeTitle
                                                               canChangeState
                                                               canDelete
                                                               artEnt
      pure $ App.render page

editArticle
  :: (ReadState m, MonadTime m, WithError m)
  => Id Article
  -> Maybe Accesstoken
  -> m HtmlPage
editArticle _     Nothing    = pure notAuthorized
editArticle artId (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.viewArticle objRef artId of
    Nothing         -> pure notAuthorized
    Just authAction -> if not $ Cap.canChangeArticleTitle objRef artId
      then pure notAuthorized
      else do
        artEnt <- Service.getArticle ctxState authAction
        let artTitle = title $ Entity.val artEnt
            link     = fieldLink Route.viewArticle artId $ Just acc
            page     = Page.editArticle artId artTitle acc link
        pure $ App.render page

invalidToken :: (Monad m) => m HtmlPage
invalidToken = pure . App.render $ Page.invalidToken

stylesheet :: (Monad m) => m Css
stylesheet = return appStylesheet
