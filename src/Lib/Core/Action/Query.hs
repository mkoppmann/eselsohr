module Lib.Core.Action.Query
  ( getCreateCollectionAcc,
    getShowArticlesAccess,
    getShowArticleAccess,
    getResourceOverviewAccs,
    getRevMap,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lib.App.Error (WithError, serverError, throwError)
import Lib.App.Log (WithLog, log, pattern D)
import Lib.Core.Domain (Accesstoken, Action (..), Article, Capability (..), Context (..), Entity (..), GetArticleActions (..), GetArticlesActions (..), Id, QueryAction (..), Reference (..), Resource, ResourceOverviewAccess (..), ResourceOverviewActions (..), Revocable, ShowArticleAccess (..), ShowArticlesAccess (..))
import Lib.Core.Domain.Accesstoken (mkAccesstoken)
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Effect (ReadCapabilities (..), ReadEntity (..))
import qualified Lib.Core.Effect.Repository as R
import UnliftIO.Async (concurrently, mapConcurrently)

getCreateCollectionAcc :: (ReadCapabilities m) => m Accesstoken
getCreateCollectionAcc = do
  (Entity capId _) <- R.getOneCap R.systemColId R.initialCapId
  pure $ capIdToAcc R.systemColId capId

getResourceOverviewAccs ::
  (ReadCapabilities m) =>
  Context ->
  ResourceOverviewActions ->
  m ResourceOverviewAccess
getResourceOverviewAccs ctx roActs = do
  let resId = resourceId $ ctxRef ctx
  let gsaaId = roaGetSharedActions roActs
  let mFcgacId = roaFrontCreateGetArticlesCap roActs
  case mFcgacId of
    Nothing -> do
      gsaaCapId <- R.getCapIdForActId resId gsaaId
      let acc1 = mkAccesstoken $ Reference resId gsaaCapId
      pure $ ResourceOverviewAccess acc1 Nothing
    Just fcgacId -> do
      (gsaaCapId, fcgacCapId) <-
        concurrently
          (R.getCapIdForActId resId gsaaId)
          (R.getCapIdForActId resId fcgacId)
      let acc1 = mkAccesstoken $ Reference resId gsaaCapId
      let acc2 = mkAccesstoken $ Reference resId fcgacCapId
      pure $ ResourceOverviewAccess acc1 $ Just acc2

getShowArticlesAccess ::
  (ReadEntity Article m, WithError m) =>
  Context ->
  GetArticlesActions ->
  m ShowArticlesAccess
getShowArticlesAccess ctx GetArticlesActions {..} = do
  let resId = resourceId $ ctxRef ctx

  gsa <- actIdToAcc resId gaaGetSharedArticlesActions
  ca <- mActIdToAcc resId gaaFrontCreateArticle
  sas <-
    Seq.reverse
      . Seq.fromList
      . catMaybes
      <$> mapConcurrently (getArtAndSAAccess resId) (Set.toList gaaShowArticles)

  pure $ ShowArticlesAccess gsa ca sas
  where
    getArtAndSAAccess ::
      (ReadEntity Article m, WithError m) =>
      Id Resource ->
      Id Action ->
      m (Maybe (Article, ShowArticleAccess))
    getArtAndSAAccess resId actId = do
      actEnt <- R.getOneAct resId actId
      case Entity.val actEnt of
        Query qAction -> getShowArticleAccess ctx qAction
        _wrongAction -> throwError $ serverError "Wrong action"

getShowArticleAccess ::
  (ReadEntity Article m, WithError m) =>
  Context ->
  QueryAction ->
  m (Maybe (Article, ShowArticleAccess))
getShowArticleAccess ctx act =
  case act of
    GetArticle artId GetArticleActions {..} -> do
      let resId = resourceId $ ctxRef ctx
      mEntity <- R.lookupEnt resId artId
      case mEntity of
        Nothing -> pure Nothing
        Just (Entity _ art) -> do
          showArticleAcc <- actIdToAcc resId gaaShowArticle
          changeArticleTitleAcc <- mActIdToAcc resId gaaChangeArticleTitle
          archiveArticleAcc <- mActIdToAcc resId gaaArchiveArticle
          unreadArticleAcc <- mActIdToAcc resId gaaUnreadArticle
          deleteArticleAcc <- mActIdToAcc resId gaaDeleteArticle
          getArticlesAcc <- mActIdToAcc resId gaaGetArticles

          let saAccess =
                ShowArticleAccess
                  showArticleAcc
                  changeArticleTitleAcc
                  archiveArticleAcc
                  unreadArticleAcc
                  deleteArticleAcc
                  getArticlesAcc

          pure $ Just (art, saAccess)
    _wrongAction -> throwError $ serverError "Wrong action"

mActIdToAcc ::
  (ReadCapabilities m) => Id Resource -> Maybe (Id Action) -> m (Maybe Accesstoken)
mActIdToAcc resId = maybe (pure Nothing) (fmap Just . actIdToAcc resId)

actIdToAcc :: (ReadCapabilities m) => Id Resource -> Id Action -> m Accesstoken
actIdToAcc resId = fmap (capIdToAcc resId) . R.getCapIdForActId resId

getRevMap ::
  (ReadCapabilities m) =>
  Context ->
  Set (Id Capability, Id Capability) ->
  m (Seq (Capability, Revocable))
getRevMap ctx capIdsSet = do
  let resId = resourceId $ ctxRef ctx

  -- Convert the set to an ascending list; this will help later for the zipping
  -- process.
  -- Not all pairs in this set are still valid.
  let capIdsList = Set.toAscList capIdsSet

  -- Use the capability id from the GetArticles action to retrieve stored
  -- capabilities and convert it to an ascending list.
  capList <- fmap Map.toAscList . getManyCap resId $ fst <$> capIdsList

  -- Filter out all deleted pairs, zip the fetched capabilities with their
  -- capability id pairs, and then create the sequence with all capability
  -- ids converted to accesstoken.
  -- The sequence is sorted based on the expiration date of the capability.
  pure
    . fmap (second (bimap (capIdToAcc resId) (capIdToAcc resId)))
    . Seq.sortBy expDateCmp
    . Seq.fromList
    . zip (snd <$> capList)
    $ filter (flip elem (fst <$> capList) . uncurry const) capIdsList
  where
    expDateCmp (cap1, _) (cap2, _) =
      compare (capExpirationDate cap1) (capExpirationDate cap2)

capIdToAcc :: Id Resource -> Id Capability -> Accesstoken
capIdToAcc resId = mkAccesstoken . Reference resId
