module Lib.Impl.Repository
  ( load
  , commit

  -- * Generic implementations
  , getOne
  , getMany
  , lookup
  , init

  -- * Specific implementations
  , insertCap
  , updateCap
  , deleteCap
  , insertAct
  , updateAct
  , deleteAct
  , insertArt
  , updateArt
  , deleteArt
  , getCapIdForActId
  , articleGetter
  , articleColInit
  , capabilityGetter
  , actionGetter

  -- Error helper
  , asSingleEntry
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Lib.App                        ( AppErrorType
                                                , WithError
                                                , WriteQueue
                                                , grab
                                                , storeError
                                                , throwOnNothing
                                                )
import           Lib.Core.Domain                ( Action
                                                , Article(..)
                                                , ArticleCollection(..)
                                                , Capability(..)
                                                , Entity(..)
                                                , Id
                                                , Resource(..)
                                                , StoreData(..)
                                                , StoreEvent(..)
                                                , SynchronizedStoreEvent(..)
                                                )
import           Lib.Impl.Repository.File       ( WithFile )
import qualified Lib.Impl.Repository.File      as File
import           Prelude                 hiding ( getAll
                                                , init
                                                )
import           UnliftIO.STM                   ( writeTQueue )

type CollectionGetter a = (Resource -> HashMap (Id a) a)

type CollectionSetter a = (Resource -> HashMap (Id a) a -> Resource)

commit
  :: (WithError m, WithFile env m) => Id Resource -> Seq StoreEvent -> m ()
commit resId storeEvents = do
  queue   <- grab @WriteQueue
  syncVar <- newEmptyTMVarIO
  let sse = SynchronizedStoreEvent resId storeEvents syncVar
  atomically $ writeTQueue queue sse
  -- Waiting for store action to finish. This needs to be in a separate atomic
  -- operation, because else we get a live lock.
  atomically $ takeTMVar syncVar

insertCap :: Id Capability -> Capability -> StoreEvent
insertCap entId newEnt = SeInsertCapability
  $ StoreData (insertSetter capabilityGetter capabilitySetter) (entId, newEnt)

updateCap :: Id Capability -> (Capability -> Capability) -> StoreEvent
updateCap entId capUpdater = SeUpdateCapability $ StoreData
  (updateSetter capabilityGetter capabilitySetter capUpdater entId)
  Prelude.id

deleteCap :: Id Capability -> StoreEvent
deleteCap entId = SeDeleteCapability
  $ StoreData (deleteSetter capabilityGetter capabilitySetter) entId

insertAct :: Id Action -> Action -> StoreEvent
insertAct entId newEnt = SeInsertAction
  $ StoreData (insertSetter actionGetter actionSetter) (entId, newEnt)

updateAct :: Id Action -> (Action -> Action) -> StoreEvent
updateAct entId actUpdater = SeUpdateAction $ StoreData
  (updateSetter actionGetter actionSetter actUpdater entId)
  Prelude.id

deleteAct :: Id Action -> StoreEvent
deleteAct entId =
  SeDeleteAction $ StoreData (deleteSetter actionGetter actionSetter) entId

insertArt :: Id Article -> Article -> StoreEvent
insertArt entId newEnt = SeInsertArticle
  $ StoreData (insertSetter articleGetter articleSetter) (entId, newEnt)

updateArt :: Id Article -> (Article -> Article) -> StoreEvent
updateArt entId artUpdater = SeUpdateArticle $ StoreData
  (updateSetter articleGetter articleSetter artUpdater entId)
  Prelude.id

deleteArt :: Id Article -> StoreEvent
deleteArt entId =
  SeDeleteArticle $ StoreData (deleteSetter articleGetter articleSetter) entId

insertSetter
  :: CollectionGetter a
  -> CollectionSetter a
  -> Resource
  -> (Id a, a)
  -> Resource
insertSetter getter setter = gsetter getter setter (uncurry Map.insert)

updateSetter
  :: CollectionGetter a
  -> CollectionSetter a
  -> (a -> a)
  -> Id a
  -> Resource
  -> (a -> a)
  -> Resource
updateSetter getter setter updater entId = gsetter getter setter setter'
  where setter' _ oldRes = Map.adjust updater entId oldRes

deleteSetter
  :: CollectionGetter a -> CollectionSetter a -> Resource -> Id a -> Resource
deleteSetter getter setter = gsetter getter setter Map.delete

getOne
  :: (WithError m) => CollectionGetter a -> Resource -> Id a -> m (Entity a)
getOne getter res = asSingleEntry . lookup getter res

getMany :: CollectionGetter a -> Resource -> HashSet (Id a) -> HashMap (Id a) a
getMany getter res entIds =
  Map.filterWithKey (\key _ -> key `Set.member` entIds) $ getter res

lookup :: CollectionGetter a -> Resource -> Id a -> Maybe (Entity a)
lookup getter res entId =
  fmap (uncurry Entity . (entId, )) . Map.lookup entId $ getter res

init :: (WithFile env m) => Id Resource -> Resource -> m ()
init = File.init

load :: (WithError m, WithFile env m) => Id Resource -> m Resource
load = flip File.load Prelude.id

getCapIdForActId :: (WithError m) => Resource -> Id Action -> m (Id Capability)
getCapIdForActId res actId =
  asSingleEntry
    . viaNonEmpty head
    . Map.keys
    . Map.filter ((==) actId . actionId)
    $ capabilityGetter res

gsetter
  :: CollectionGetter a
  -> CollectionSetter a
  -> (b -> HashMap (Id a) a -> HashMap (Id a) a)
  -> Resource
  -> b
  -> Resource
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
  ArticleResource $ artCol { artCollection = newVal }

capabilityGetter :: CollectionGetter Capability
capabilityGetter (ArticleResource artCol) = artCapCollection artCol

capabilitySetter :: CollectionSetter Capability
capabilitySetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol { artCapCollection = newVal }

actionGetter :: CollectionGetter Action
actionGetter (ArticleResource artCol) = artActCollection artCol

actionSetter :: CollectionSetter Action
actionSetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol { artActCollection = newVal }

-- Error helpers

asSingleEntry :: (WithError m) => Maybe a -> m a
asSingleEntry = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Expected a single entry, but got none"
