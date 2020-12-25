module Lib.Web.Handler.Frontend
  ( frontendServer,
  )
where

import Lib.App (WithError, invalid, missingParameter, serverError, throwError)
import Lib.Core.Accesstoken (Accesstoken (..))
import Lib.Core.Article (Article (..))
import Lib.Core.Capability (Capability (..))
import Lib.Core.Collection (Collection)
import Lib.Core.Id (Id, castId)
import Lib.Core.UserAction (ArticleAction (..), CollectionAction (..), UserAction (..))
import Lib.Effect.Log (WithLog, log, pattern D)
import Lib.Effect.Random (MonadRandom (..))
import Lib.Effect.Resource (QueryEntity (..), RWEntity)
import qualified Lib.Effect.Resource as Res
import qualified Lib.Web.Handler.Common as HC
import qualified Lib.Web.Route as Route
import Lib.Web.Types (AppServer, ArticleWithTokens (..), HtmlPage)
import qualified Lib.Web.View.App as App
import qualified Lib.Web.View.Page as Page
import Lib.Web.View.Style (Css, appStylesheet)

frontendServer :: Route.FrontendSite AppServer
frontendServer =
  Route.FrontendSite
    { Route.startpage = startpage,
      Route.collectionMain = collectionMain,
      Route.collectionSettings = collectionSettings,
      Route.collectionShare = collectionShare,
      Route.listArticles = listArticles,
      Route.showArticle = showArticle,
      Route.editArticle = editArticle,
      Route.stylesheet = stylesheet
    }

startpage ::
  ( RWEntity Accesstoken m,
    RWEntity Capability m,
    MonadRandom m,
    WithError m,
    WithLog env m
  ) =>
  m HtmlPage
startpage = prepareFirstCap >>= renderPage
  where
    prepareFirstCap = do
      log D "Initializing database"
      HC.initAccDb Res.accDbId
      log D "Database created"
      let createCap = UaColAction CreateCollection
      log D "Looking for initial capabilities"
      mCap <- Res.capLookupViaAction Res.accDbId createCap
      case mCap of
        Just cap -> do
          log D "Initial capabilities already exist. Using them."
          HC.getAccIdViaCapId Res.accDbId $ capabilityId cap
        Nothing -> do
          log D "We need to create our initial capabilities."
          HC.addCapAndAcc Res.accDbId Nothing createCap

    renderPage accId = do
      let page = Page.root HC.actionR accId
      pure $ App.render HC.stylesheetR page

collectionMain ::
  ( RWEntity Accesstoken m,
    RWEntity Capability m,
    MonadRandom m,
    WithError m,
    WithLog env m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
collectionMain mAccId = case mAccId of
  Nothing -> throwError $ missingParameter "Acc parameter missing"
  Just accId -> do
    (collId, action) <- HC.getCollIdAndActionViaAccId Res.accDbId accId
    case action of
      UaArtAction _notCollectionAction -> throwError $ invalid "Wrong action"
      UaColAction collectionAction -> case collectionAction of
        OverviewCollection -> do
          -- TODO: Get tokens for collection settings and share menu
          listArticlesCapId <- Res.capGetOneViaAction collId $ UaArtAction ListArticles
          listArticlesAccIds <- HC.getAccIdsViaCapId Res.accDbId $ capabilityId listArticlesCapId
          deleteAccIds <- HC.getAccIdsForAction collId Nothing $ UaColAction DeleteListArticles
          let activeLinks = zip listArticlesAccIds deleteAccIds
          newListArticleCap <- Res.capGetOneViaAction collId $ UaColAction CreateListArticlesAcc
          newListArticlesAcc <- HC.addAcc collId $ capabilityId newListArticleCap
          let page =
                Page.collectionMain
                  (HC.collectionSettingsR Nothing)
                  (HC.collectionShareR Nothing)
                  HC.actionR
                  (HC.listArticlesR Nothing)
                  activeLinks
                  newListArticlesAcc
          pure $ App.render HC.stylesheetR page
        _wrongCollectionAction -> throwError $ invalid "Wrong action"

collectionSettings :: (WithError m) => Maybe (Id Accesstoken) -> m HtmlPage
collectionSettings mAccId = throwError $ serverError "Action not yet implemented"

collectionShare :: (WithError m) => Maybe (Id Accesstoken) -> m HtmlPage
collectionShare mAccId = throwError $ serverError "Action not yet implemented"

listArticles ::
  ( QueryEntity Accesstoken m,
    QueryEntity Article m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
listArticles mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    ListArticles -> do
      insertArticleToken <- HC.getAccIdForAction collId Nothing $ UaArtAction InsertArticle
      articlesWithTokens <- getArticlesForRendering collId
      let page =
            Page.articles
              (HC.showArticleR Nothing)
              (HC.editArticleR Nothing)
              HC.actionR
              insertArticleToken
              articlesWithTokens
      pure $ App.render HC.stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

showArticle ::
  ( QueryEntity Accesstoken m,
    QueryEntity Article m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
showArticle mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    ShowArticle aId -> do
      articleWithTokens <- getArticleForRendering collId aId
      let page = Page.showArticle (HC.editArticleR Nothing) HC.actionR articleWithTokens
      pure $ App.render HC.stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

editArticle ::
  ( QueryEntity Accesstoken m,
    QueryEntity Article m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
editArticle mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    EditArticle aId -> do
      article <- Res.getOne collId aId
      let changeCap = UaArtAction $ ChangeArticleTitle aId
      editTitleCap <- HC.getAccIdForAction collId (Just $ castId @() aId) changeCap
      let page = Page.editArticle HC.actionR editTitleCap $ articleTitle article
      pure $ App.render HC.stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

genericArticleHandler ::
  ( QueryEntity Accesstoken m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m (Id Collection, ArticleAction)
genericArticleHandler mAccId = case mAccId of
  Nothing -> throwError $ missingParameter "Acc parameter missing"
  Just accId -> do
    (collId, action) <- HC.getCollIdAndActionViaAccId Res.accDbId accId
    case action of
      UaColAction _notArticleAction -> throwError $ invalid "Wrong action"
      UaArtAction articleAction -> pure (collId, articleAction)

stylesheet :: (Monad m) => m Css
stylesheet = return appStylesheet

getArticlesForRendering ::
  ( QueryEntity Accesstoken m,
    QueryEntity Article m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  m [ArticleWithTokens]
getArticlesForRendering collId =
  Res.getMany collId
    >>= traverse (getArticleForRendering collId . articleId)

getArticleForRendering ::
  ( QueryEntity Accesstoken m,
    QueryEntity Article m,
    QueryEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  Id Article ->
  m ArticleWithTokens
getArticleForRendering collId aId = do
  article <- Res.getOne collId aId
  showToken <- tokenFor $ ShowArticle aId
  editToken <- tokenFor $ EditArticle aId
  changeTitleToken <- tokenFor $ ChangeArticleTitle aId
  archiveToken <- tokenFor $ ArchiveArticle aId
  unreadToken <- tokenFor $ UnreadArticle aId
  deleteToken <- tokenFor $ DeleteArticle aId
  return $
    ArticleWithTokens
      article
      showToken
      editToken
      changeTitleToken
      archiveToken
      unreadToken
      deleteToken
  where
    tokenFor =
      HC.getAccIdForAction collId (Just $ castId @() aId) . UaArtAction
