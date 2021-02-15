module Lib.Core.Action.Query
  ( getShowArticlesAccess,
    getShowArticleAccess,
    getResourceOverviewAccs,
    getRevMap,
  )
where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Lib.App.Error (WithError, serverError, throwError)
import Lib.App.Log (WithLog, log, pattern E)
import Lib.Core.Domain.Accesstoken (Accesstoken, Reference (..), Revocable, mkAccesstoken)
import Lib.Core.Domain.Article (Article (..))
import Lib.Core.Domain.Capability (Action (..), Capability (..), GetArticleActions (..), GetArticlesActions (..), QueryAction (..), ResourceOverviewActions (..))
import Lib.Core.Domain.Context (Context (..))
import Lib.Core.Domain.Entity (Entity (..))
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Domain.ExpirationDate (ExpirationDate (..))
import Lib.Core.Domain.Frontend (ResourceOverviewAccess (..), ShowArticleAccess (..), ShowArticlesAccess (..))
import Lib.Core.Domain.Id (Id)
import Lib.Core.Domain.Resource (Resource)
import Lib.Core.Effect.Repository (ReadCapabilities (..), ReadEntity (..))
import qualified Lib.Core.Effect.Repository as R
import Lib.Core.Effect.Time (MonadTime (..))
import UnliftIO.Async (mapConcurrently)

getResourceOverviewAccs ::
  (ReadCapabilities m, WithError m, WithLog env m) =>
  Context ->
  ResourceOverviewActions ->
  m ResourceOverviewAccess
getResourceOverviewAccs ctx roActs = do
  let resId = resourceId $ ctxRef ctx
  case roaFrontCreateGetArticlesCap roActs of
    Nothing -> do
      log E "roaFrontCreateGetArticlesCap is missing"
      throwError $ serverError "A system error occured."
    Just fcgacId -> do
      fcgacCapId <- R.getCapIdForActId resId fcgacId
      let acc = mkAccesstoken $ Reference resId fcgacCapId
      pure . ResourceOverviewAccess $ Just acc

getShowArticlesAccess ::
  (ReadEntity Article m, WithError m) =>
  Context ->
  GetArticlesActions ->
  m ShowArticlesAccess
getShowArticlesAccess ctx GetArticlesActions {..} = do
  let resId = resourceId $ ctxRef ctx

  ca <- mActIdToAcc resId gaaFrontCreateArticle
  sas <-
    fromList
      . sortBy (flip expDateCmp)
      . catMaybes
      <$> mapConcurrently (getArtAndSAAccess resId) (Set.toList gaaShowArticles)

  pure $ ShowArticlesAccess ca sas
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

    expDateCmp ::
      (Article, ShowArticleAccess) -> (Article, ShowArticleAccess) -> Ordering
    expDateCmp (art1, _) (art2, _) = compare (creation art1) (creation art2)

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
  (ReadCapabilities m, MonadTime m) =>
  Context ->
  HashSet (Id Capability, Id Capability) ->
  m (Seq (Capability, Revocable))
getRevMap ctx capIdsSet = do
  let resId = resourceId $ ctxRef ctx

  currTime <- getCurrentTime

  -- Convert the set to an ascending list; this will help later for the zipping
  -- process.
  -- Not all pairs in this set are still valid.
  let capIdsList = Set.toList capIdsSet

  -- Use the capability id from the GetArticles action to retrieve stored
  -- capabilities and convert it to an ascending list.
  capList <- fmap Map.toList . getManyCap resId $ fst <$> capIdsList

  -- Filter out all deleted pairs, zip the fetched capabilities with their
  -- capability id pairs, and then create the sequence with all capability
  -- ids converted to accesstoken.
  -- The sequence is sorted based on the expiration date of the capability.
  pure
    . fmap (second (bimap (capIdToAcc resId) (capIdToAcc resId)))
    . fromList
    . sortBy expDateCmp
    . filter (expDateFilter currTime)
    . zip (snd <$> capList)
    $ filter (flip elem (fst <$> capList) . uncurry const) capIdsList
  where
    expDateFilter currTime (cap, _) = case capExpirationDate cap of
      Nothing -> True
      Just expDate -> unExpirationDate expDate > currTime
    expDateCmp (cap1, _) (cap2, _) =
      compare (capExpirationDate cap1) (capExpirationDate cap2)

capIdToAcc :: Id Resource -> Id Capability -> Accesstoken
capIdToAcc resId = mkAccesstoken . Reference resId
