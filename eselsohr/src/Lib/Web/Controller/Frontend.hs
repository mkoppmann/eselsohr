module Lib.Web.Controller.Frontend
  (
  -- * API
    frontend
  ,

  -- * Handlers
    startpage
  ) where

import           Clay                           ( Css )
import qualified Data.HashSet                  as Set
import           Data.Time                      ( UTCTime(..)
                                                , addGregorianMonthsClip
                                                )
import           Lib.App                        ( pattern E
                                                , pattern I
                                                , WithError
                                                , WithLog
                                                , log
                                                , notFound
                                                , redirect303
                                                , serverError
                                                , throwError
                                                )
import qualified Lib.Core.Action               as Action
import qualified Lib.Core.Action.Query         as Query
import           Lib.Core.Domain                ( Accesstoken
                                                , Action(..)
                                                , Article(..)
                                                , Capability
                                                , Context(..)
                                                , ExpirationDate(..)
                                                , GetArticlesActions
                                                , Id
                                                , QueryAction(..)
                                                , ResourceOverviewAccess
                                                , ResourceOverviewActions(..)
                                                , Revocable
                                                , ShowArticleAccess(..)
                                                )

import qualified Lib.Core.Domain.Entity        as Entity

import qualified Lib.Core.Domain.Uri           as Uri
import           Lib.Core.Effect                ( ContextState(..)
                                                , MonadRandom
                                                , MonadScraper
                                                , MonadTime(..)
                                                , RWState
                                                , ReadState
                                                , WriteState
                                                )
import qualified Lib.Core.Flow                 as Flow
import qualified Lib.Web.Controller.Common     as CC
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( AppServer
                                                , DeleteActionForm(..)
                                                , HtmlPage
                                                , PatchActionForm(..)
                                                , PostActionForm(..)
                                                , Redirection
                                                )
import qualified Lib.Web.View.App              as App
import qualified Lib.Web.View.Page             as Page
import           Lib.Web.View.Style             ( appStylesheet )
import           Servant                        ( Link )

frontend :: Route.FrontendSite AppServer
frontend = Route.FrontendSite { Route.startpage      = startpage
                              , Route.collectionMain = collectionMain
                              , Route.listArticles   = listArticles
                              , Route.showArticle    = showArticle
                              , Route.editArticle    = editArticle
                              , Route.invalidToken   = invalidToken
                              , Route.stylesheet     = stylesheet
                              , Route.createResource = createResource
                              , Route.deleteFrontend = deleteFrontend
                              , Route.patchFrontend  = patchFrontend
                              , Route.postFrontend   = postFrontend
                              }

startpage :: (Monad m) => m HtmlPage
startpage = pure $ App.render Page.root

collectionMain
  :: (ReadState m, MonadTime m, WithError m, WithLog env m)
  => Maybe Accesstoken
  -> m HtmlPage
collectionMain Nothing    = pure notAuthorized
collectionMain (Just acc) = CC.getContextState acc >>= \case
  Left err -> do
    log I err
    throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
  Right ctx -> do
    dates   <- getExpirationDates

    roaActs <- getActions . Entity.val . ctxAct $ csContext ctx
    let gagacId = roaGetActiveGetArticlesCap roaActs
    gagac <- CC.getAction ctx gagacId
    let capIdsSet = extractSet gagac
    (accMap, revMap) <- fetchData ctx roaActs capIdsSet

    pure . App.render $ Page.resourceOverview acc dates accMap revMap
 where
  getExpirationDates :: (MonadTime m) => m (ExpirationDate, ExpirationDate)
  getExpirationDates = do
    currTime@UTCTime {..} <- getCurrentTime
    let expDate = currTime { utctDay = addGregorianMonthsClip 1 utctDay }
    pure (ExpirationDate currTime, ExpirationDate expDate)

  getActions :: (WithError m) => Action -> m ResourceOverviewActions
  getActions (Query qAction) = case qAction of
    ResourceOverview roActs -> pure roActs
    _wrongQueryAction ->
      throwError $ serverError "The accesstoken did not include actions"
  getActions _wrongAction =
    throwError $ serverError "The accesstoken did not include actions"

  extractSet :: Action -> HashSet (Id Capability, Id Capability)
  extractSet (Query qAction) = case qAction of
    GetActiveGetArticlesCaps capIdsSet -> capIdsSet
    _wrongQueryAction                  -> Set.empty
  extractSet _wrongAction = Set.empty

  fetchData
    :: (MonadTime m, WithError m, WithLog env m)
    => ContextState
    -> ResourceOverviewActions
    -> HashSet (Id Capability, Id Capability)
    -> m (ResourceOverviewAccess, Seq (Capability, Revocable))
  fetchData ctx roActs capIdsSet = do
    accMap <- Query.getResourceOverviewAccs ctx roActs
    revMap <- Query.getRevMap ctx capIdsSet
    pure (accMap, revMap)

notAuthorized :: HtmlPage
notAuthorized = App.render Page.notAuthorized

listArticles
  :: (ReadState m, MonadTime m, WithError m, WithLog env m)
  => Maybe Accesstoken
  -> m HtmlPage
listArticles Nothing    = pure notAuthorized
listArticles (Just acc) = CC.getContextState acc >>= \case
  Left err -> do
    log I err
    throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
  Right ctx -> do
    laActs <- getActions . Entity.val . ctxAct $ csContext ctx
    laAcc  <- Query.getShowArticlesAccess ctx laActs

    pure . App.render $ Page.articleList acc laAcc
 where
  getActions :: (WithError m) => Action -> m GetArticlesActions
  getActions (Query qAction) = case qAction of
    GetArticles laActs -> pure laActs
    _wrongQueryAction ->
      throwError $ serverError "ListArticles received the wrong query action."
  getActions _wrongAction =
    throwError $ serverError "The accesstoken did not include actions"

showArticle
  :: (ReadState m, MonadTime m, WithError m, WithLog env m)
  => Maybe Accesstoken
  -> m HtmlPage
showArticle Nothing    = pure notAuthorized
showArticle (Just acc) = CC.getContextState acc >>= \case
  Left err -> do
    log I err
    throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
  Right ctx -> do
    saQAction <- getGetArticleAction . Entity.val . ctxAct $ csContext ctx
    Query.getShowArticleAccess ctx saQAction >>= \case
      Nothing           -> throwError notFound
      Just (art, saAcc) -> pure . App.render $ Page.showArticle art saAcc

getGetArticleAction :: (WithError m) => Action -> m QueryAction
getGetArticleAction (Query qAction) = case qAction of
  GetArticle _artId _saActs -> pure qAction
  _wrongQueryAction         -> do
    throwError $ serverError "ShowArticle received the wrong query action."
getGetArticleAction _wrongAction =
  throwError $ serverError "The accesstoken did not include actions"

editArticle
  :: (ReadState m, MonadTime m, WithError m, WithLog env m)
  => Maybe Accesstoken
  -> m HtmlPage
editArticle Nothing    = pure notAuthorized
editArticle (Just acc) = CC.getContextState acc >>= \case
  Left err -> do
    log I err
    throwError . redirect303 $ Route.linkAsText Route.invalidTokenR
  Right ctx -> do
    edQAction <- getGetArticleAction . Entity.val . ctxAct $ csContext ctx
    Query.getShowArticleAccess ctx edQAction >>= \case
      Nothing -> throwError notFound
      Just (Article aTitle _ _ _, ShowArticleAccess {..}) ->
        case saAccChangeArticleTitle of
          Nothing -> pure notAuthorized
          Just caAcc ->
            pure . App.render . Page.editArticle aTitle caAcc $ showArticleLink
              saAccShowArticle
 where
  showArticleLink :: Accesstoken -> Link
  showArticleLink = Route.showArticleR . Just

invalidToken :: (Monad m) => m HtmlPage
invalidToken = pure . App.render $ Page.invalidToken

stylesheet :: (Monad m) => m Css
stylesheet = return appStylesheet

createResource :: (WriteState m, MonadRandom m, WithError m) => m Redirection
createResource = throwError . redirect303 =<< Action.createResource

deleteFrontend
  :: (RWState m, MonadTime m, WithError m, WithLog env m)
  => DeleteActionForm
  -> m Redirection
deleteFrontend (DeleteActionForm accId gotoUrl) =
  CC.getContextState accId >>= \case
    Left  err -> log E err >> throwError (serverError err)
    Right ctx -> do
      Flow.deleteAction ctx
      throwError . redirect303 $ Uri.render gotoUrl

patchFrontend
  :: (RWState m, MonadTime m, WithError m, WithLog env m)
  => PatchActionForm
  -> m Redirection
patchFrontend (PatchActionForm accId gotoUrl artTitle) =
  CC.getContextState accId >>= \case
    Left  err -> log E err >> throwError (serverError err)
    Right ctx -> do
      Flow.patchAction ctx artTitle
      throwError . redirect303 $ Uri.render gotoUrl

postFrontend
  :: ( RWState m
     , MonadRandom m
     , MonadScraper m
     , MonadTime m
     , WithError m
     , WithLog env m
     )
  => PostActionForm
  -> m Redirection
postFrontend (PostActionForm accId gotoUrl artUri unlockPetname expirationDate)
  = CC.getContextState accId >>= \case
    Left  err -> log E err >> throwError (serverError err)
    Right ctx -> do
      Flow.postAction ctx artUri unlockPetname expirationDate >>= \case
        Nothing  -> throwError . redirect303 $ Uri.render gotoUrl
        Just url -> throwError $ redirect303 url
