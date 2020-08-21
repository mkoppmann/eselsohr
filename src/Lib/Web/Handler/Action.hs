module Lib.Web.Handler.Action
  ( actionServer,
  )
where

import Lib.App (WithError, invalid, missingParameter, redirect, serverError, throwError)
import Lib.Core.Accesstoken (Accesstoken (..))
import Lib.Core.Article (Article (..), ArticleState (..))
import Lib.Core.Capability (Capability (..))
import Lib.Core.Collection (Collection)
import Lib.Core.Id (Id, castId)
import Lib.Core.Uri (Uri, render)
import Lib.Core.UserAction (ArticleAction (..), CollectionAction (..), UserAction (..))
import Lib.Db.Wrapper (WithDb)
import Lib.Effect.Log (WithLog, log, pattern D, pattern E)
import Lib.Effect.Random (MonadRandom (..))
import Lib.Effect.Resource (CommandAccesstoken (..), CommandArticle (..), CommandCollection (..), QueryAccesstoken (..), QueryCollection (..), RWAccesstoken, RWCollection)
import Lib.Effect.Scraper (MonadScraper (..))
import Lib.Effect.Time (MonadTime (..))
import Lib.Web.Handler.Common (addCapAndAccesstoken, getAccIdForAction, linkAsText, listArticlesR, showArticleR)
import qualified Lib.Web.Route.Action as Route
import Lib.Web.Types (AppServer, DeleteActionForm (..), PatchActionForm (..), PostActionForm (..), Redirection)

actionServer :: Route.ActionSite AppServer
actionServer =
  Route.ActionSite
    { Route.deleteAction = deleteAction,
      Route.patchAction = patchAction,
      Route.postAction = postAction
    }

genericActionHandler ::
  ( QueryAccesstoken m,
    QueryCollection m
  ) =>
  Id Accesstoken ->
  m (Id Collection, UserAction)
genericActionHandler accId = do
  acc <- getAccesstoken accId
  let collId = accesstokenCol acc
  let capId = accesstokenCap acc
  cap <- getCapability collId capId
  let capAction = capabilityAction cap
  pure (collId, capAction)

deleteAction ::
  ( RWAccesstoken m,
    CommandArticle m,
    RWCollection m,
    WithError m
  ) =>
  DeleteActionForm ->
  m Redirection
deleteAction DeleteActionForm {..} = do
  (collId, capAction) <- genericActionHandler deleteafAccesstoken
  case capAction of
    UaArticleAction articleAction -> case articleAction of
      DeleteArticle aId -> handleDeleteArticle collId aId
      _nonDeleteAction -> throwError $ invalid "Wrong action"
    _nonDeleteAction -> throwError $ invalid "Wrong action"
  where
    handleDeleteArticle collId aId = do
      deleteAllArticleCaps collId aId
      deleteArticle collId aId
      let listCap = UaArticleAction ListArticles
      listArticles <- getAccIdForAction collId Nothing listCap
      throwError . redirect . linkAsText . listArticlesR $ Just listArticles

patchAction ::
  ( RWAccesstoken m,
    CommandArticle m,
    RWCollection m,
    WithError m
  ) =>
  PatchActionForm ->
  m Redirection
patchAction PatchActionForm {..} = do
  (collId, capAction) <- genericActionHandler patchafAccesstoken
  case capAction of
    UaArticleAction articleAction -> case articleAction of
      ChangeArticleTitle aId -> handleChangeArticleTitle collId aId patchafArticleTitle
      ArchiveArticle aId -> handleArchiveArticle collId aId
      UnreadArticle aId -> handleUnreadArticle collId aId
      _nonPatchAction -> throwError $ invalid "Wrong action"
    _nonPatchAction -> throwError $ invalid "Wrong action"
  where
    handleChangeArticleTitle collId aId mNewTitle = do
      case mNewTitle of
        Nothing -> throwError $ missingParameter "New title is missing"
        Just newTitle -> do
          changeArticleTitle collId aId newTitle
          let showCap = UaArticleAction $ ShowArticle aId
          showArticle <- getAccIdForAction collId Nothing showCap
          throwError . redirect . linkAsText . showArticleR $ Just showArticle

    handleArchiveArticle collId aId = do
      archiveArticle collId aId
      let listCap = UaArticleAction ListArticles
      listArticles <- getAccIdForAction collId Nothing listCap
      throwError . redirect . linkAsText . listArticlesR $ Just listArticles

    handleUnreadArticle collId aId = do
      unreadArticle collId aId
      let listCap = UaArticleAction ListArticles
      listArticles <- getAccIdForAction collId Nothing listCap
      throwError . redirect . linkAsText . listArticlesR $ Just listArticles

postAction ::
  ( RWAccesstoken m,
    CommandArticle m,
    RWCollection m,
    MonadRandom m,
    MonadScraper m,
    MonadTime m,
    WithDb env m,
    WithError m,
    WithLog env m
  ) =>
  PostActionForm ->
  m Redirection
postAction PostActionForm {..} = do
  (collId, capAction) <- genericActionHandler postafAccesstoken
  case capAction of
    UaArticleAction articleAction -> case articleAction of
      InsertArticle -> handleInsertArticle collId postafUri
      _nonPostAction -> throwError $ invalid "Wrong action"
    UaCollectionAction collectionArticle -> case collectionArticle of
      CreateCollection -> handleCreateCollection
  where
    handleInsertArticle collId mUri = do
      case mUri of
        Nothing -> throwError $ missingParameter "URI is missing"
        Just uri -> do
          article <- createArticle uri
          insertArticle collId article
          createArticleCaps collId $ articleId article
          log D $ "New article for url " <> toText (render uri) <> " was created."
          let listCap = UaArticleAction ListArticles
          listArticles <- getAccIdForAction collId Nothing listCap
          throwError . redirect . linkAsText . listArticlesR $ Just listArticles

    handleCreateCollection = do
      capId <- createCollection
      log D $ "New collection with id " <> show capId <> " was created."
      throwError . redirect . linkAsText . listArticlesR $ Just capId

addArticleCapAndAcc ::
  ( CommandAccesstoken m,
    CommandCollection m,
    MonadRandom m
  ) =>
  Id Collection ->
  Maybe (Id Article) ->
  ArticleAction ->
  m (Maybe (Id Accesstoken))
addArticleCapAndAcc collId mAId articleAction =
  case articleAction of
    InsertArticle ->
      addCapAndAccesstoken collId Nothing userAction
        >>= (return . pure)
    ListArticles ->
      addCapAndAccesstoken collId Nothing userAction
        >>= (return . pure)
    ShowArticle actionId -> compareIdsAndAddCaps actionId mAId
    EditArticle actionId -> compareIdsAndAddCaps actionId mAId
    ChangeArticleTitle actionId -> compareIdsAndAddCaps actionId mAId
    ArchiveArticle actionId -> compareIdsAndAddCaps actionId mAId
    UnreadArticle actionId -> compareIdsAndAddCaps actionId mAId
    DeleteArticle actionId -> compareIdsAndAddCaps actionId mAId
  where
    userAction = UaArticleAction articleAction

    addCap articleId =
      addCapAndAccesstoken collId (Just $ castId @() articleId) userAction
        >>= (return . pure)

    compareIdsAndAddCaps aId1 = \case
      (Just aId2) -> if aId1 == aId2 then addCap aId1 else pure Nothing
      Nothing -> pure Nothing

deleteAllArticleCaps ::
  ( CommandAccesstoken m,
    RWCollection m
  ) =>
  Id Collection ->
  Id Article ->
  m ()
deleteAllArticleCaps collId aId = do
  articleCaps <- getAllCapabilitiesViaEntity collId $ castId @() aId
  let articleCapIds = fmap capabilityId articleCaps
  traverse_ deleteAccesstokenViaCapability articleCapIds
  traverse_ (deleteCapability collId) articleCapIds

createArticleCaps ::
  ( CommandAccesstoken m,
    CommandCollection m,
    MonadRandom m
  ) =>
  Id Collection ->
  Id Article ->
  m ()
createArticleCaps collId aId =
  let addCap action = addArticleCapAndAcc collId (Just aId) $ action aId
      caps =
        [ ShowArticle,
          EditArticle,
          ChangeArticleTitle,
          ArchiveArticle,
          UnreadArticle,
          DeleteArticle
        ]
   in traverse_ addCap caps

createArticle ::
  ( MonadRandom m,
    MonadScraper m,
    MonadTime m
  ) =>
  Uri ->
  m Article
createArticle aUri = do
  aId <- getRandomId
  aTitle <- scrapWebsite aUri
  aCreated <- getCurrentTime
  let aState = Unread
  return $ Article aId aTitle aUri aState aCreated

createCollection ::
  ( CommandAccesstoken m,
    CommandCollection m,
    MonadRandom m,
    WithError m,
    WithLog env m
  ) =>
  m (Id Accesstoken)
createCollection = do
  collId <- getRandomId
  createCollectionTables collId
  accId <- addCap collId ListArticles
  _ <- addCap collId InsertArticle
  pure accId
  where
    addCap collId cap = do
      mArticleCap <- addArticleCapAndAcc collId Nothing cap
      case mArticleCap of
        Nothing -> do
          log E $
            "Couldn’t create capability "
              <> show cap
              <> "for collection "
              <> show collId
          throwError $ serverError "Could not create new collection"
        Just articleCap -> pure articleCap
