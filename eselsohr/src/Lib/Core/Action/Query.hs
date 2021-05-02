module Lib.Core.Action.Query
  ( getShowArticlesAccess
  , getShowArticleAccess
  , getResourceOverviewAccs
  , getRevMap
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Lib.App                        ( pattern E
                                                , WithError
                                                , WithLog
                                                , log
                                                , serverError
                                                , throwError
                                                )
import           Lib.Core.Domain                ( Accesstoken
                                                , Action(..)
                                                , Article(..)
                                                , Capability(..)
                                                , Context(..)
                                                , Entity(..)
                                                , ExpirationDate(..)
                                                , GetArticleActions(..)
                                                , GetArticlesActions(..)
                                                , Id
                                                , QueryAction(..)
                                                , Reference(..)
                                                , Resource
                                                , ResourceOverviewAccess(..)
                                                , ResourceOverviewActions(..)
                                                , Revocable
                                                , ShowArticleAccess(..)
                                                , ShowArticlesAccess(..)
                                                , mkAccesstoken
                                                )
import qualified Lib.Core.Domain.Entity        as Entity
import           Lib.Core.Effect                ( ContextState(..)
                                                , MonadTime(..)
                                                , SealedResource
                                                )
import qualified Lib.Core.Effect.Repository    as R

import           Relude.Extra                   ( secondF )

getResourceOverviewAccs
  :: (WithError m, WithLog env m)
  => ContextState
  -> ResourceOverviewActions
  -> m ResourceOverviewAccess
getResourceOverviewAccs ctx roActs = do
  let resId = ctxResId $ csContext ctx
      res   = csResource ctx
  case roaCreateGetArticlesCap roActs of
    Nothing -> do
      log E "roaCreateGetArticlesCap is missing"
      throwError $ serverError "A system error occured."
    Just fcgacId ->
      ResourceOverviewAccess . Just <$> actIdToAcc resId res fcgacId

getShowArticlesAccess
  :: (WithError m) => ContextState -> GetArticlesActions -> m ShowArticlesAccess
getShowArticlesAccess ctx GetArticlesActions {..} = do
  let resId = ctxResId $ csContext ctx
      res   = csResource ctx
  ca  <- mActIdToAcc resId res gaaCreateArticle
  sas <- fromList . sortBy (flip expDateCmp) . catMaybes <$> traverse
    (getArtAndSAAccess res)
    (Set.toList gaaShowArticles)
  pure $ ShowArticlesAccess ca sas
 where
  getArtAndSAAccess
    :: (WithError m)
    => SealedResource
    -> Id Action
    -> m (Maybe (Article, ShowArticleAccess))
  getArtAndSAAccess res actId = do
    actEnt <- R.getOneAct res actId
    case Entity.val actEnt of
      Query qAction -> getShowArticleAccess ctx qAction
      _wrongAction  -> throwError $ serverError "Wrong action"

  expDateCmp
    :: (Article, ShowArticleAccess) -> (Article, ShowArticleAccess) -> Ordering
  expDateCmp (art1, _) (art2, _) = compare (creation art1) (creation art2)

getShowArticleAccess
  :: (WithError m)
  => ContextState
  -> QueryAction
  -> m (Maybe (Article, ShowArticleAccess))
getShowArticleAccess ctx (GetArticle artId GetArticleActions {..}) = do
  let resId = ctxResId $ csContext ctx
      res   = csResource ctx

  case R.lookupArt res artId of
    Nothing             -> pure Nothing
    Just (Entity _ art) -> do
      showArticleAcc        <- actIdToAcc resId res gaaShowArticle
      changeArticleTitleAcc <- mActIdToAcc resId res gaaChangeArticleTitle
      archiveArticleAcc     <- mActIdToAcc resId res gaaArchiveArticle
      unreadArticleAcc      <- mActIdToAcc resId res gaaUnreadArticle
      deleteArticleAcc      <- mActIdToAcc resId res gaaDeleteArticle
      getArticlesAcc        <- mActIdToAcc resId res gaaGetArticles

      let saAccess = ShowArticleAccess showArticleAcc
                                       changeArticleTitleAcc
                                       archiveArticleAcc
                                       unreadArticleAcc
                                       deleteArticleAcc
                                       getArticlesAcc

      pure $ Just (art, saAccess)
getShowArticleAccess _ctx _wrongAction =
  throwError $ serverError "Wrong action"

getRevMap
  :: (MonadTime m)
  => ContextState
  -> HashSet (Id Capability, Id Capability)
  -> m (Seq (Capability, Revocable))
getRevMap ctx capIdsSet = do
  let resId = ctxResId $ csContext ctx
      res   = csResource ctx

  currTime <- getCurrentTime

  -- Convert the set to a list; this will help later for the zipping
  -- process. Not all pairs in this set are still valid.
  let capIdsList = Set.toList capIdsSet

  -- Use the capability id from the GetArticles action to retrieve stored
  -- capabilities and convert it to an ascending list.
  let capList = Map.toList . R.getManyCap res $ Set.map fst capIdsSet

  -- Filter out all deleted pairs, zip the fetched capabilities with their
  -- capability id pairs, and then create the sequence with all capability
  -- ids converted to accesstoken.
  -- The sequence is sorted based on the expiration date of the capability.
  pure
    . secondF (bimap (capIdToAcc resId) (capIdToAcc resId))
    . fromList
    . sortBy expDateCmp
    . filter (expDateFilter currTime)
    . zip (snd <$> capList)
    $ filter (flip elem (fst <$> capList) . uncurry const) capIdsList
 where
  expDateFilter currTime (cap, _) = case capExpirationDate cap of
    Nothing      -> True
    Just expDate -> unExpirationDate expDate > currTime
  expDateCmp (cap1, _) (cap2, _) =
    compare (capExpirationDate cap1) (capExpirationDate cap2)

mActIdToAcc
  :: (WithError m)
  => Id Resource
  -> SealedResource
  -> Maybe (Id Action)
  -> m (Maybe Accesstoken)
mActIdToAcc resId res = maybe (pure Nothing) (fmap Just . actIdToAcc resId res)

actIdToAcc
  :: (WithError m)
  => Id Resource
  -> SealedResource
  -> Id Action
  -> m Accesstoken
actIdToAcc resId res actId = capIdToAcc resId <$> R.getCapIdForActId res actId

capIdToAcc :: Id Resource -> Id Capability -> Accesstoken
capIdToAcc resId = mkAccesstoken . Reference resId
