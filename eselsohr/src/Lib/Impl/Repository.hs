module Lib.Impl.Repository
  ( commit,

    -- * Generic implementations
    getOne,
    getMany,
    lookup,
    init,

    -- * Specific implementations
    insertCap,
    updateCap,
    deleteCap,
    insertAct,
    updateAct,
    deleteAct,
    insertArt,
    updateArt,
    deleteArt,
    getCapIdForActId,
    articleGetter,
    articleColInit,
    capabilityGetter,
    actionGetter,
    -- Error helper
    asSingleEntry,
  )
where

import qualified Data.HashMap.Strict as Map
import Lib.App (AppErrorType, WithError, WriteQueue, grab, storeError, throwOnNothing)
import Lib.Core.Domain.Article (Article (..))
import qualified Lib.Core.Domain.Article as Article
import Lib.Core.Domain.Capability (Action, Capability (..))
import Lib.Core.Domain.Entity (Entity (..))
import Lib.Core.Domain.Id (Id)
import Lib.Core.Domain.Resource (ArticleCollection (..), Resource (..))
import Lib.Core.Domain.StoreEvent (StoreData (..), StoreEvent (..), SynchronizedStoreEvent (..))
import Lib.Impl.Repository.File (WithFile)
import qualified Lib.Impl.Repository.File as File
import UnliftIO.STM (writeTQueue)
import Prelude hiding (getAll, init)

type CollectionGetter a = (Resource -> HashMap (Id a) a)

type CollectionSetter a = (Resource -> HashMap (Id a) a -> Resource)

commit ::
  (WithError m, WithFile env m) => Id Resource -> Seq StoreEvent -> m ()
commit resId storeEvents = do
  queue <- grab @WriteQueue
  syncVar <- newEmptyTMVarIO
  let sse = SynchronizedStoreEvent resId storeEvents syncVar
  atomically $ writeTQueue queue sse
  -- Waiting for store action to finish.
  -- This needs to be in a separate atomic operation, because else we get a
  -- live lock.
  atomically $ takeTMVar syncVar

insertCap :: Id Capability -> Capability -> StoreEvent
insertCap entId newEnt =
  SeInsertCapability $
    StoreData
      (insertSetter capabilityGetter capabilitySetter)
      (entId, newEnt)

updateCap :: Id Capability -> Capability -> StoreEvent
updateCap entId newEnt =
  SeUpdateCapability $
    StoreData
      (updateSetter capabilityGetter capabilitySetter capUpdater entId)
      newEnt
  where
    capUpdater :: Capability -> Capability
    capUpdater _ = newEnt

deleteCap :: Id Capability -> StoreEvent
deleteCap entId =
  SeDeleteCapability $
    StoreData
      (deleteSetter capabilityGetter capabilitySetter)
      entId

insertAct :: Id Action -> Action -> StoreEvent
insertAct entId newEnt =
  SeInsertAction $
    StoreData
      (insertSetter actionGetter actionSetter)
      (entId, newEnt)

updateAct :: Id Action -> Action -> StoreEvent
updateAct entId newEnt = do
  SeUpdateAction $
    StoreData
      (updateSetter actionGetter actionSetter actUpdater entId)
      newEnt
  where
    actUpdater :: Action -> Action
    actUpdater _ = newEnt

deleteAct :: Id Action -> StoreEvent
deleteAct entId =
  SeDeleteAction $
    StoreData
      (deleteSetter actionGetter actionSetter)
      entId

insertArt :: Id Article -> Article -> StoreEvent
insertArt entId newEnt =
  SeInsertArticle $
    StoreData
      (insertSetter articleGetter articleSetter)
      (entId, newEnt)

updateArt :: Id Article -> Article -> StoreEvent
updateArt entId newEnt =
  SeUpdateArticle $
    StoreData
      (updateSetter articleGetter articleSetter artUpdater entId)
      newEnt
  where
    -- Only the title and the state of an 'Article' can be changed.
    artUpdater :: Article -> Article
    artUpdater oldArt =
      oldArt
        { Article.title = Article.title newEnt,
          Article.state = Article.state newEnt
        }

deleteArt :: Id Article -> StoreEvent
deleteArt entId =
  SeDeleteArticle $
    StoreData
      (deleteSetter articleGetter articleSetter)
      entId

insertSetter ::
  CollectionGetter a -> CollectionSetter a -> Resource -> (Id a, a) -> Resource
insertSetter getter setter = gsetter getter setter (uncurry Map.insert)

updateSetter ::
  CollectionGetter a ->
  CollectionSetter a ->
  (a -> a) ->
  Id a ->
  Resource ->
  a ->
  Resource
updateSetter getter setter updater entId = gsetter getter setter setter'
  where
    setter' _ oldRes = Map.adjust updater entId oldRes

deleteSetter ::
  CollectionGetter a -> CollectionSetter a -> Resource -> Id a -> Resource
deleteSetter getter setter = gsetter getter setter Map.delete

getOne ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  Id a ->
  m (Entity a)
getOne getter resId = asSingleEntry <=< lookup getter resId

getMany ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  [Id a] ->
  m (HashMap (Id a) a)
getMany getter resId entIds =
  Map.filterWithKey (\key _ -> key `elem` entIds) <$> getAll getter resId

lookup ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  Id a ->
  m (Maybe (Entity a))
lookup getter resId entId = do
  aMap <- getAll getter resId
  let ent = Map.lookup entId aMap
  let mTuple = (entId,) <$> ent
  pure $ fmap (uncurry Entity) mTuple

init ::
  (WithFile env m) =>
  Id Resource ->
  Resource ->
  m ()
init = File.init

getAll ::
  (WithError m, WithFile env m) =>
  CollectionGetter a ->
  Id Resource ->
  m (HashMap (Id a) a)
getAll = flip File.load

getCapIdForActId ::
  (WithError m, WithFile env m) =>
  Id Resource ->
  Id Action ->
  m (Id Capability)
getCapIdForActId resId actId = do
  allCaps <- getAll capabilityGetter resId
  let capList = Map.toList $ Map.filter ((==) actId . actionId) allCaps
  let capIdList = fst <$> capList
  asSingleEntry $ viaNonEmpty head capIdList

gsetter ::
  CollectionGetter a ->
  CollectionSetter a ->
  (b -> HashMap (Id a) a -> HashMap (Id a) a) ->
  Resource ->
  b ->
  Resource
gsetter fromCol toCol setter oldRes val =
  toCol oldRes . setter val $ fromCol oldRes

-- * 'Resource' specific getters and setters

articleColInit :: Resource
articleColInit =
  ArticleResource $ ArticleCollection Map.empty Map.empty Map.empty

articleGetter :: CollectionGetter Article
articleGetter (ArticleResource artCol) = artCollection artCol

articleSetter :: CollectionSetter Article
articleSetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol {artCollection = newVal}

capabilityGetter :: CollectionGetter Capability
capabilityGetter (ArticleResource artCol) = artCapCollection artCol

capabilitySetter :: CollectionSetter Capability
capabilitySetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol {artCapCollection = newVal}

actionGetter :: CollectionGetter Action
actionGetter (ArticleResource artCol) = artActCollection artCol

actionSetter :: CollectionSetter Action
actionSetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol {artActCollection = newVal}

-- Error helpers

asSingleEntry :: (WithError m) => Maybe a -> m a
asSingleEntry = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Expected a single entry, but got none"
