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
                                                , CreateSharedArticleRefPerms(..)
                                                , CreateSharedArticlesRefPerms(..)
                                                , CreateSharedOverviewRefPerms(..)
                                                , HtmlPage
                                                , ShareArticleLinkData(..)
                                                , ShareArticleListLinkData(..)
                                                , ShareOverviewLinkData(..)
                                                , ViewArticleData(..)
                                                )
import qualified Lib.Web.View.App              as App
import qualified Lib.Web.View.Page             as Page
import           Lib.Web.View.Style             ( appStylesheet )
import           Servant                        ( fieldLink )

frontend :: Route.FrontendSite AppServer
frontend = Route.FrontendSite
  { Route.startpage               = startpage
  , Route.invalidToken            = invalidToken
  , Route.collectionOverview      = collectionOverview
  , Route.shareCollectionOverview = shareCollectionOverview
  , Route.viewArticles            = viewArticles
  , Route.shareViewArticles       = shareViewArticles
  , Route.newArticle              = newArticle
  , Route.viewArticle             = viewArticle
  , Route.shareViewArticle        = shareViewArticle
  , Route.editArticle             = editArticle
  , Route.stylesheet              = stylesheet
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
      let canCreateUnlockLink = Cap.canCreateUnlockLink objRef
          canShareLinks = Cap.canManageSharedRef objRef
          page = Page.collectionOverview $ CollectionOverviewData { .. }
      pure $ App.render page

shareCollectionOverview
  :: (ReadState m, MonadTime m, WithError m) => Maybe Accesstoken -> m HtmlPage
shareCollectionOverview Nothing    = pure notAuthorized
shareCollectionOverview (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.viewSharedRefs objRef of
    Nothing                  -> pure notAuthorized
    Just sharedRefAuthAction -> do
      sharedLinks <- Service.getSharedRefs ctxState
                                           Cap.isSharedOverviewRef
                                           sharedRefAuthAction
      (earliestExpDate, defaultExpDate) <- getExpirationDates
      let canViewUnlockLinks   = Cap.canGetUnlockLinks objRef
          canCreateUnlockLinks = Cap.canCreateUnlockLink objRef
          canDeleteUnlockLinks = Cap.canDeleteUnlockLink objRef
          sharingPerms         = CreateSharedOverviewRefPerms { .. }
          page = Page.shareOverviewLink $ ShareOverviewLinkData { .. }
      pure $ App.render page

viewArticles
  :: (ReadState m, MonadTime m, WithError m) => Maybe Accesstoken -> m HtmlPage
viewArticles Nothing    = pure notAuthorized
viewArticles (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.viewArticles objRef of
    Nothing            -> pure notAuthorized
    Just artAuthAction -> do
      articles <- Service.getArticles ctxState artAuthAction
      let canCreateArticles     = Cap.canCreateArticle objRef
          canChangeArticleTitle = Cap.canChangeAllArticleTitles objRef
          canChangeArticleState = Cap.canChangeAllArticleStates objRef
          canDeleteArticle      = Cap.canDeleteAllArticles objRef
          canShareLinks         = Cap.canManageSharedRef objRef
          page                  = Page.articleList $ ArticleListData { .. }
      pure $ App.render page

shareViewArticles
  :: (ReadState m, MonadTime m, WithError m) => Maybe Accesstoken -> m HtmlPage
shareViewArticles Nothing    = pure notAuthorized
shareViewArticles (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.viewSharedRefs objRef of
    Nothing                  -> pure notAuthorized
    Just sharedRefAuthAction -> do
      sharedLinks <- Service.getSharedRefs ctxState
                                           Cap.isSharedArticlesRef
                                           sharedRefAuthAction
      (earliestExpDate, defaultExpDate) <- getExpirationDates
      let canViewArticles       = Cap.canViewArticles objRef
          canCreateArticles     = Cap.canCreateArticle objRef
          canChangeArticleTitle = Cap.canChangeAllArticleTitles objRef
          canChangeArticleState = Cap.canChangeAllArticleStates objRef
          canDeleteArticle      = Cap.canDeleteAllArticles objRef
          sharingPerms          = CreateSharedArticlesRefPerms { .. }
          page = Page.shareArticleListLink $ ShareArticleListLinkData { .. }
      pure $ App.render page

newArticle
  :: (ReadState m, MonadTime m, WithError m) => Maybe Accesstoken -> m HtmlPage
newArticle Nothing    = pure notAuthorized
newArticle (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
      link   = fieldLink Route.newArticle $ Just acc
      page   = Page.newArticlePage acc link
  if not $ Cap.canCreateArticle objRef
    then pure notAuthorized
    else pure $ App.render page

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
      article <- Service.getArticle ctxState authAction
      let canViewArticles       = Cap.canViewArticles objRef
          canChangeArticleTitle = Cap.canChangeArticleTitle objRef artId
          canChangeArticleState = Cap.canChangeArticleState objRef artId
          canDeleteArticle      = Cap.canDeleteArticle objRef artId
          canShareLinks         = Cap.canManageSharedRef objRef
          page                  = Page.showArticle $ ViewArticleData { .. }
      pure $ App.render page

shareViewArticle
  :: (ReadState m, MonadTime m, WithError m)
  => Id Article
  -> Maybe Accesstoken
  -> m HtmlPage
shareViewArticle _     Nothing    = pure notAuthorized
shareViewArticle artId (Just acc) = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState
  case Cap.viewSharedRefs objRef of
    Nothing                  -> pure notAuthorized
    Just sharedRefAuthAction -> do
      sharedLinks <- Service.getSharedRefs ctxState
                                           Cap.isSharedArticleRef
                                           sharedRefAuthAction
      (earliestExpDate, defaultExpDate) <- getExpirationDates
      let canViewArticle        = Cap.canViewArticle objRef artId
          canChangeArticleTitle = Cap.canChangeArticleTitle objRef artId
          canChangeArticleState = Cap.canChangeArticleState objRef artId
          canDeleteArticle      = Cap.canDeleteArticle objRef artId
          sharingPerms          = CreateSharedArticleRefPerms { .. }
          page = Page.shareArticleLink artId $ ShareArticleLinkData { .. }
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

getExpirationDates :: (MonadTime m) => m (ExpirationDate, ExpirationDate)
getExpirationDates = do
  currTime@UTCTime {..} <- getCurrentTime
  let expDate = currTime { utctDay = addGregorianMonthsClip 1 utctDay }
  pure (ExpirationDate currTime, ExpirationDate expDate)
