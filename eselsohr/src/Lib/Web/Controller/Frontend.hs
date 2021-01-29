module Lib.Web.Controller.Frontend
  ( frontend,
  )
where

import Clay (Css)
import qualified Data.Set as Set
import Data.Time (UTCTime (..), addGregorianMonthsClip)
import Lib.App.Error (WithError, invalid, notFound, redirect303, redirect307, serverError, throwError)
import Lib.App.Log (WithLog, log, pattern E, pattern I)
import qualified Lib.Core.Action as Action
import qualified Lib.Core.Action.Query as Query
import Lib.Core.Domain.Accesstoken (Accesstoken, Revocable, mkAccesstoken)
import Lib.Core.Domain.Article (Article (..))
import Lib.Core.Domain.Capability (Action (..), Capability, GetArticlesActions, QueryAction (..), ResourceOverviewActions (..))
import Lib.Core.Domain.Context (Context (..))
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Domain.ExpirationDate (ExpirationDate (..))
import Lib.Core.Domain.Frontend (ResourceOverviewAccess, ShowArticleAccess (..))
import Lib.Core.Domain.Id (Id)
import Lib.Core.Effect.Random (MonadRandom)
import Lib.Core.Effect.Repository (RWEntity, ReadCapabilities, ReadEntity)
import Lib.Core.Effect.Scraper (MonadScraper)
import Lib.Core.Effect.Time (MonadTime (..))
import qualified Lib.Core.Flow as Flow
import qualified Lib.Web.Controller.Common as CC
import qualified Lib.Web.Route as Route
import Lib.Web.Types (AppServer, DeleteActionForm (..), HtmlPage, PatchActionForm (..), PostActionForm (..), Redirection)
import qualified Lib.Web.View.App as App
import qualified Lib.Web.View.Page as Page
import Lib.Web.View.Style (appStylesheet)
import Lucid (Html)
import UnliftIO (concurrently)

frontend :: Route.FrontendSite AppServer
frontend =
  Route.FrontendSite
    { Route.startpage = startpage,
      Route.collectionMain = collectionMain,
      Route.listArticles = listArticles,
      Route.showArticle = showArticle,
      Route.editArticle = editArticle,
      Route.invalidToken = invalidToken,
      Route.stylesheet = stylesheet,
      Route.createResource = createResource,
      Route.deleteFrontend = deleteFrontend,
      Route.patchFrontend = patchFrontend,
      Route.postFrontend = postFrontend
    }

startpage :: (ReadCapabilities m) => m HtmlPage
startpage = pure $ App.render Page.root

collectionMain ::
  (ReadEntity Article m, MonadTime m, WithError m, WithLog env m) =>
  Maybe Accesstoken ->
  m HtmlPage
collectionMain mAcc = case mAcc of
  Nothing -> pure notAuthorized
  Just acc -> do
    mOrigCtx <- CC.getContext acc
    case mOrigCtx of
      Left err -> do
        log I err
        throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
      Right origCtx -> do
        ctx <- CC.getQuery origCtx
        let viewAcc = mkAccesstoken $ ctxRef ctx
        currTime@UTCTime {..} <- getCurrentTime
        let expDate = addGregorianMonthsClip 1 utctDay
        let dates =
              ( ExpirationDate currTime,
                ExpirationDate $ currTime {utctDay = expDate}
              )
        roaActs <- getActions . Entity.val $ ctxAct ctx
        let gagacId = roaGetActiveGetArticlesCap roaActs
        gagac <- CC.getAction ctx gagacId
        let capIdsSet = extractSet gagac
        (accMap, revMap) <- fetchData ctx roaActs capIdsSet
        pure $ renderPage viewAcc dates accMap revMap
  where
    getActions :: (WithError m) => Action -> m ResourceOverviewActions
    getActions = \case
      Query qAction -> case qAction of
        ResourceOverview roActs -> pure roActs
        _wrongQueryAction ->
          throwError $ serverError "The accesstoken did not include actions"
      _wrongAction ->
        throwError $ serverError "The accesstoken did not include actions"

    extractSet :: Action -> Set (Id Capability, Id Capability)
    extractSet = \case
      Query qAction -> case qAction of
        GetActiveGetArticlesCaps capIdsSet -> capIdsSet
        _wrongQueryAction -> Set.empty
      _wrongAction -> Set.empty

    fetchData ctx roActs capIdsSet =
      concurrently
        (Query.getResourceOverviewAccs ctx roActs)
        (Query.getRevMap ctx capIdsSet)

    renderPage :: Accesstoken -> (ExpirationDate, ExpirationDate) -> ResourceOverviewAccess -> Seq (Capability, Revocable) -> Html ()
    renderPage viewAcc dates x y = App.render $ Page.resourceOverview viewAcc dates x y

notAuthorized :: HtmlPage
notAuthorized = App.render Page.notAuthorized

listArticles ::
  (ReadEntity Article m, MonadTime m, WithError m, WithLog env m) =>
  Maybe Accesstoken ->
  m HtmlPage
listArticles mAcc = case mAcc of
  Nothing -> pure notAuthorized
  Just acc -> do
    mOrigCtx <- CC.getContext acc
    case mOrigCtx of
      Left err -> do
        log I err
        throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
      Right origCtx -> do
        ctx <- CC.getQuery origCtx
        let viewAcc = mkAccesstoken $ ctxRef ctx
        laActs <- getActions . Entity.val $ ctxAct ctx
        laAcc <- Query.getShowArticlesAccess ctx laActs
        pure . App.render $ Page.articleList viewAcc laAcc
  where
    getActions :: (WithError m) => Action -> m GetArticlesActions
    getActions = \case
      Query qAction -> case qAction of
        GetArticles laActs -> pure laActs
        _wrongQueryAction -> do
          throwError $ serverError "ListArticles received the wrong query action."
      _wrongAction ->
        throwError $ serverError "The accesstoken did not include actions"

showArticle ::
  (ReadEntity Article m, MonadTime m, WithError m, WithLog env m) =>
  Maybe Accesstoken ->
  m HtmlPage
showArticle = \case
  Nothing -> pure notAuthorized
  Just acc -> do
    mOrigCtx <- CC.getContext acc
    case mOrigCtx of
      Left err -> do
        log I err
        throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
      Right origCtx -> do
        ctx <- CC.getQuery origCtx
        saQAction <- getGetArticleAction . Entity.val $ ctxAct ctx
        mSaAcc <- Query.getShowArticleAccess ctx saQAction
        case mSaAcc of
          Nothing -> throwError notFound
          Just (art, saAcc) -> pure . App.render $ Page.showArticle art saAcc

getGetArticleAction :: (WithError m) => Action -> m QueryAction
getGetArticleAction = \case
  Query qAction -> case qAction of
    GetArticle _artId _saActs -> pure qAction
    _wrongQueryAction -> do
      throwError $ serverError "ShowArticle received the wrong query action."
  _wrongAction ->
    throwError $ serverError "The accesstoken did not include actions"

editArticle ::
  (ReadEntity Article m, MonadTime m, WithError m, WithLog env m) =>
  Maybe Accesstoken ->
  m HtmlPage
editArticle = \case
  Nothing -> pure notAuthorized
  Just acc -> do
    mOrigCtx <- CC.getContext acc
    case mOrigCtx of
      Left err -> do
        log I err
        throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
      Right origCtx -> do
        ctx <- CC.getQuery origCtx
        edQAction <- getGetArticleAction . Entity.val $ ctxAct ctx
        mEdAcc <- Query.getShowArticleAccess ctx edQAction
        case mEdAcc of
          Nothing -> throwError notFound
          Just (Article aTitle _ _ _, ShowArticleAccess {..}) ->
            case saAccChangeArticleTitle of
              Nothing -> pure notAuthorized
              Just caAcc -> pure . App.render $ Page.editArticle aTitle caAcc

invalidToken :: (Monad m) => m HtmlPage
invalidToken = pure . App.render $ Page.invalidToken

stylesheet :: (Monad m) => m Css
stylesheet = return appStylesheet

createResource ::
  (RWEntity Article m, MonadRandom m, WithError m) =>
  m Redirection
createResource = do
  url <- Action.createResource
  redirect307To $ Just url

deleteFrontend ::
  (RWEntity Article m, MonadTime m, WithError m, WithLog env m) =>
  DeleteActionForm ->
  m Redirection
deleteFrontend (DeleteActionForm accId) = do
  mOrigCtx <- CC.getContext accId
  case mOrigCtx of
    Left err -> log E err >> throwError (serverError err)
    Right origCtx -> do
      ctx <- CC.getCommand origCtx
      Flow.deleteAction ctx
      redirect307To . CC.getRedirectTo . Entity.val $ ctxAct origCtx

patchFrontend ::
  (RWEntity Article m, MonadTime m, WithError m, WithLog env m) =>
  PatchActionForm ->
  m Redirection
patchFrontend (PatchActionForm accId artTitle) = do
  mOrigCtx <- CC.getContext accId
  case mOrigCtx of
    Left err -> log E err >> throwError (serverError err)
    Right origCtx -> do
      ctx <- CC.getCommand origCtx
      Flow.patchAction ctx artTitle
      redirect307To . CC.getRedirectTo . Entity.val $ ctxAct origCtx

postFrontend ::
  ( RWEntity Article m,
    MonadRandom m,
    MonadScraper m,
    MonadTime m,
    WithError m,
    WithLog env m
  ) =>
  PostActionForm ->
  m Redirection
postFrontend (PostActionForm accId artUri unlockPetname expirationDate) = do
  mOrigCtx <- CC.getContext accId
  case mOrigCtx of
    Left err -> log E err >> throwError (serverError err)
    Right origCtx -> do
      ctx <- CC.getCommand origCtx
      mUrl <- Flow.postAction ctx artUri unlockPetname expirationDate
      case mUrl of
        Nothing ->
          redirect307To . CC.getRedirectTo . Entity.val $ ctxAct origCtx
        Just url -> redirect307To $ Just url

redirect307To :: (WithError m) => Maybe Text -> m Redirection
redirect307To = \case
  Nothing ->
    throwError $ invalid "Handled a non-frontend action in the frontend controller"
  Just url -> throwError $ redirect307 url
