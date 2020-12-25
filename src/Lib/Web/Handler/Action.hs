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
import Lib.Effect.Log (WithLog, log, pattern D)
import Lib.Effect.Random (MonadRandom (..))
import Lib.Effect.Resource (CommandEntity (..), QueryEntity (..), RWEntity)
import qualified Lib.Effect.Resource as Res
import Lib.Effect.Scraper (MonadScraper (..))
import Lib.Effect.Time (MonadTime (..))
import qualified Lib.Web.Handler.Common as HC
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
  ( QueryEntity Accesstoken m,
    QueryEntity Capability m
  ) =>
  Id Accesstoken ->
  m (Id Collection, UserAction)
genericActionHandler accId = do
  acc <- Res.getOne Res.accDbId accId
  let collId = accesstokenCol acc
  let capId = accesstokenCap acc
  cap <- Res.getOne collId capId
  let capAction = capabilityAction cap
  pure (collId, capAction)

deleteAction ::
  ( RWEntity Accesstoken m,
    CommandEntity Article m,
    RWEntity Capability m,
    WithError m
  ) =>
  DeleteActionForm ->
  m Redirection
deleteAction DeleteActionForm {..} = do
  (collId, capAction) <- genericActionHandler deleteafAccesstoken
  case capAction of
    UaArtAction articleAction -> case articleAction of
      DeleteArticle aId -> handleDeleteArticle collId aId
      _nonDeleteAction -> throwError $ invalid "Wrong action"
    UaColAction collectionAction -> case collectionAction of
      DeleteListArticles -> handleDeleteListArticles collId
      _nonDeleteAction -> throwError $ invalid "Wrong action"
  where
    handleDeleteListArticles collId = do
      acc <- Res.getOne Res.accDbId deleteafAccesstoken
      let capId = accesstokenCap acc
      cap <- Res.getOne collId capId
      case capabilityEntity cap of
        Nothing -> throwError $ serverError "Couldn’t find matching accesstoken"
        Just listArticlesAccId -> do
          listArticlesAcc <- Res.getOne Res.accDbId $ castId @Accesstoken listArticlesAccId
          Res.delete Res.accDbId $ accesstokenId listArticlesAcc
          Res.delete Res.accDbId deleteafAccesstoken
          Res.delete collId $ capabilityId cap
          -- Collection Main Link necessary
          throwError . redirect . HC.linkAsText $ HC.collectionMainR Nothing

    handleDeleteArticle collId aId = do
      deleteAllArticleCaps collId aId
      Res.delete collId aId
      let listCap = UaArtAction ListArticles
      listArticles <- HC.getAccIdForAction collId Nothing listCap
      throwError . redirect . HC.linkAsText . HC.listArticlesR $ Just listArticles

patchAction ::
  ( RWEntity Accesstoken m,
    RWEntity Article m,
    RWEntity Capability m,
    WithError m
  ) =>
  PatchActionForm ->
  m Redirection
patchAction PatchActionForm {..} = do
  (collId, capAction) <- genericActionHandler patchafAccesstoken
  case capAction of
    UaArtAction articleAction -> case articleAction of
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
          Res.artUpdateTitle collId aId newTitle
          let showCap = UaArtAction $ ShowArticle aId
          showArticle <- HC.getAccIdForAction collId Nothing showCap
          throwError . redirect . HC.linkAsText . HC.showArticleR $ Just showArticle

    handleArchiveArticle collId aId = do
      Res.artUpdateState collId aId Archived
      let listCap = UaArtAction ListArticles
      listArticles <- HC.getAccIdForAction collId Nothing listCap
      throwError . redirect . HC.linkAsText . HC.listArticlesR $ Just listArticles

    handleUnreadArticle collId aId = do
      Res.artUpdateState collId aId Unread
      let listCap = UaArtAction ListArticles
      listArticles <- HC.getAccIdForAction collId Nothing listCap
      throwError . redirect . HC.linkAsText . HC.listArticlesR $ Just listArticles

postAction ::
  ( RWEntity Accesstoken m,
    CommandEntity Article m,
    RWEntity Capability m,
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
    UaArtAction articleAction -> case articleAction of
      InsertArticle -> handleInsertArticle collId postafUri
      _nonPostArticleAction -> throwError $ invalid "Wrong action"
    UaColAction collectionArticle -> case collectionArticle of
      CreateCollection -> handleCreateCollection
      CreateListArticlesAcc -> handleListArticlesAcc collId
      _nonPostCollectionAction -> throwError $ invalid "Wrong action"
  where
    handleInsertArticle collId mUri = do
      case mUri of
        Nothing -> throwError $ missingParameter "URI is missing"
        Just uri -> do
          article <- createArticle uri
          Res.insert collId article
          createArticleCaps collId $ articleId article
          log D $ "New article for url " <> toText (render uri) <> " was created."
          let listCap = UaArtAction ListArticles
          listArticles <- HC.getAccIdForAction collId Nothing listCap
          throwError . redirect . HC.linkAsText . HC.listArticlesR $ Just listArticles

    handleCreateCollection = do
      collectionMainAcc <- createCollection
      log D "New collection was created."
      throwError . redirect . HC.linkAsText . HC.collectionMainR $ Just collectionMainAcc

    handleListArticlesAcc collId = do
      listArticlesAcc <- unlockCollection collId
      log D $ "New ListArticles cap with accesstoken " <> show listArticlesAcc <> " was created."
      Res.delete Res.accDbId postafAccesstoken
      log D $ "Accesstoken " <> show postafAccesstoken <> " was deleted."
      throwError . redirect . HC.linkAsText . HC.listArticlesR $ Just listArticlesAcc

addArticleCapAndAcc ::
  ( CommandEntity Accesstoken m,
    CommandEntity Capability m,
    MonadRandom m,
    WithLog env m
  ) =>
  Id Collection ->
  Maybe (Id Article) ->
  ArticleAction ->
  m (Maybe (Id Accesstoken))
addArticleCapAndAcc collId mAId articleAction =
  case articleAction of
    InsertArticle ->
      HC.addCapAndAcc collId Nothing userAction
        >>= (return . pure)
    ListArticles ->
      HC.addCapAndAcc collId Nothing userAction
        >>= (return . pure)
    ShowArticle actionId -> compareIdsAndAddCaps actionId mAId
    EditArticle actionId -> compareIdsAndAddCaps actionId mAId
    ChangeArticleTitle actionId -> compareIdsAndAddCaps actionId mAId
    ArchiveArticle actionId -> compareIdsAndAddCaps actionId mAId
    UnreadArticle actionId -> compareIdsAndAddCaps actionId mAId
    DeleteArticle actionId -> compareIdsAndAddCaps actionId mAId
  where
    userAction = UaArtAction articleAction

    addCap articleId =
      HC.addCapAndAcc collId (Just $ castId @() articleId) userAction
        >>= (return . pure)

    compareIdsAndAddCaps aId1 = \case
      (Just aId2) -> if aId1 == aId2 then addCap aId1 else pure Nothing
      Nothing -> pure Nothing

deleteAllArticleCaps ::
  ( RWEntity Accesstoken m,
    RWEntity Capability m,
    WithError m
  ) =>
  Id Collection ->
  Id Article ->
  m ()
deleteAllArticleCaps collId aId = do
  articleCaps <- Res.capGetManyViaEntity collId $ castId @() aId
  let articleCapIds = fmap capabilityId articleCaps
  traverse_ (Res.accDeleteViaCap Res.accDbId) articleCapIds
  traverse_ (Res.delete collId) articleCapIds

createArticleCaps ::
  ( CommandEntity Accesstoken m,
    CommandEntity Capability m,
    MonadRandom m,
    WithLog env m
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

unlockCollection ::
  ( RWEntity Accesstoken m,
    RWEntity Capability m,
    MonadRandom m,
    WithError m,
    WithLog env m
  ) =>
  Id Collection ->
  m (Id Accesstoken)
unlockCollection collId = do
  listArticlesCap <- Res.capGetOneViaAction collId $ UaArtAction ListArticles
  insertArticlesCap <- Res.capGetOneViaAction collId $ UaArtAction InsertArticle
  listArticlesAccId <- HC.addAcc collId $ capabilityId listArticlesCap
  _ <- HC.addAcc collId $ capabilityId insertArticlesCap
  _ <- HC.addCapAndAcc collId (Just $ castId @() listArticlesAccId) $ UaColAction DeleteListArticles
  pure listArticlesAccId

createCollection ::
  ( CommandEntity Accesstoken m,
    CommandEntity Article m,
    RWEntity Capability m,
    MonadRandom m,
    WithLog env m
  ) =>
  m (Id Accesstoken)
createCollection = do
  collId <- getRandomId
  HC.initCollDb collId
  _ <- HC.addCap collId Nothing $ UaColAction CreateListArticlesAcc
  _ <- HC.addCap collId Nothing $ UaArtAction ListArticles
  _ <- HC.addCap collId Nothing $ UaArtAction InsertArticle
  HC.addCapAndAcc collId Nothing $ UaColAction OverviewCollection
