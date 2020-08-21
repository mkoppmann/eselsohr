module Lib.Web.Handler.Frontend
  ( frontendServer,
  )
where

import Lib.App.Error (WithError, invalid, missingParameter, throwError)
import Lib.Core.Accesstoken (Accesstoken (..))
import Lib.Core.Article (Article (..))
import Lib.Core.Capability (Capability (..))
import Lib.Core.Collection (Collection)
import Lib.Core.Id (Id, castId)
import Lib.Core.UserAction (ArticleAction (..), CollectionAction (..), UserAction (..))
import Lib.Effect.Random (MonadRandom (..))
import Lib.Effect.Resource (CommandAccesstoken (..), QueryAccesstoken (..), QueryArticle (..), QueryCollection (..), RWAccesstoken, RWCollection, accDbId)
import Lib.Web.Handler.Common (actionR, addCapAndAccesstoken, editArticleR, getAccIdForAction, showArticleR, stylesheetR)
import qualified Lib.Web.Route as Route
import Lib.Web.Types (AppServer, ArticleWithTokens (..), HtmlPage)
import qualified Lib.Web.View.App as App
import qualified Lib.Web.View.Page as Page
import Lib.Web.View.Style (Css, appStylesheet)

frontendServer :: Route.FrontendSite AppServer
frontendServer =
  Route.FrontendSite
    { Route.startpage = startpage,
      Route.listArticles = listArticles,
      Route.showArticle = showArticle,
      Route.editArticle = editArticle,
      Route.stylesheet = stylesheet
    }

startpage ::
  ( RWAccesstoken m,
    RWCollection m,
    MonadRandom m
  ) =>
  m HtmlPage
startpage = prepareFirstCap >>= renderPage
  where
    prepareFirstCap = do
      createAccesstokenTable
      let collId = castId @Collection accDbId
      let createCap = UaCollectionAction CreateCollection
      mCap <- lookupCapabilityViaAction collId createCap
      case mCap of
        Just cap -> getAccId $ capabilityId cap
        Nothing -> addCapAndAccesstoken collId Nothing createCap

    getAccId capId = accesstokenId <$> getAccesstokenViaCapability capId

    renderPage accId = do
      let page = Page.root actionR accId
      pure $ App.render stylesheetR page

listArticles ::
  ( QueryAccesstoken m,
    QueryArticle m,
    QueryCollection m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
listArticles mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    ListArticles -> do
      insertArticleToken <- getAccIdForAction collId Nothing $ UaArticleAction InsertArticle
      articlesWithTokens <- getArticlesForRendering collId
      let page =
            Page.articles
              (showArticleR Nothing)
              (editArticleR Nothing)
              actionR
              insertArticleToken
              articlesWithTokens
      pure $ App.render stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

showArticle ::
  ( QueryAccesstoken m,
    QueryArticle m,
    QueryCollection m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
showArticle mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    ShowArticle aId -> do
      articleWithTokens <- getArticleForRendering collId aId
      let page = Page.showArticle (editArticleR Nothing) actionR articleWithTokens
      pure $ App.render stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

editArticle ::
  ( QueryAccesstoken m,
    QueryArticle m,
    QueryCollection m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m HtmlPage
editArticle mAccId = do
  (collId, articleAction) <- genericArticleHandler mAccId
  case articleAction of
    EditArticle aId -> do
      article <- getArticle collId aId
      let changeCap = UaArticleAction $ ChangeArticleTitle aId
      editTitleCap <- getAccIdForAction collId (Just $ castId @() aId) changeCap
      let page = Page.editArticle actionR editTitleCap $ articleTitle article
      pure $ App.render stylesheetR page
    _otherAction -> throwError $ invalid "Wrong action"

genericArticleHandler ::
  ( QueryAccesstoken m,
    QueryCollection m,
    WithError m
  ) =>
  Maybe (Id Accesstoken) ->
  m (Id Collection, ArticleAction)
genericArticleHandler mAccId = case mAccId of
  Nothing -> throwError $ missingParameter "Acc parameter missing"
  Just accId -> do
    (collId, action) <- getCollIdAndActionFromAccId accId
    case action of
      UaCollectionAction _notArticleAction -> throwError $ invalid "Wrong action"
      UaArticleAction articleAction -> pure (collId, articleAction)

stylesheet :: (Monad m) => m Css
stylesheet = return appStylesheet

getCollIdAndActionFromAccId ::
  ( QueryAccesstoken m,
    QueryCollection m
  ) =>
  Id Accesstoken ->
  m (Id Collection, UserAction)
getCollIdAndActionFromAccId accId = do
  acc <- getAccesstoken accId
  let collId = accesstokenCol acc
  cap <- getCapability collId (accesstokenCap acc)
  pure . (,) collId $ capabilityAction cap

getArticlesForRendering ::
  ( QueryAccesstoken m,
    QueryArticle m,
    QueryCollection m,
    WithError m
  ) =>
  Id Collection ->
  m [ArticleWithTokens]
getArticlesForRendering collId =
  getArticles collId
    >>= traverse (getArticleForRendering collId . articleId)

getArticleForRendering ::
  ( QueryAccesstoken m,
    QueryArticle m,
    QueryCollection m,
    WithError m
  ) =>
  Id Collection ->
  Id Article ->
  m ArticleWithTokens
getArticleForRendering collId aId = do
  article <- getArticle collId aId
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
      getAccIdForAction collId (Just $ castId @() aId) . UaArticleAction
