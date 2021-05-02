module Lib.Impl.Repository
  ( SealedResource

  -- * File specific functions
  , init
  , load
  , commit

  -- * Domain type implementations
  , initArtCol
  , articleColInit
  , articleGetter
  , lookupArt
  , getManyArt
  , getAllArt
  , insertArt
  , updateArt
  , deleteArt
  , artUpdateTitle
  , artUpdateState
  , actionGetter
  , lookupAct
  , getManyAct
  , getAllAct
  , insertAct
  , updateAct
  , deleteAct
  , capabilityGetter
  , lookupCap
  , getManyCap
  , getAllCap
  , insertCap
  , updateCap
  , deleteCap
  , getCapIdForActId
  , lookupCapIdForActId

  -- * Helpers
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
                                                , ArticleState
                                                , Capability(..)
                                                , Entity(..)
                                                , Id
                                                , Resource(..)
                                                , StoreData(..)
                                                , StoreEvent(..)
                                                , SynchronizedStoreEvent(..)
                                                )
import qualified Lib.Core.Domain.Article       as Article
import           Lib.Impl.Repository.File       ( WithFile )
import qualified Lib.Impl.Repository.File      as File
import           Prelude                 hiding ( getAll
                                                , init
                                                )
import           UnliftIO.STM                   ( writeTQueue )

newtype SealedResource = SealedResource {unSealResource :: Resource}

type CollectionGetter a = (Resource -> HashMap (Id a) a)

type CollectionSetter a = (Resource -> HashMap (Id a) a -> Resource)

-- * File specific functions

init :: (WithFile env m) => Id Resource -> Resource -> m ()
init = File.init

load :: (WithError m, WithFile env m) => Id Resource -> m SealedResource
load = pure . SealedResource <=< flip File.load Prelude.id

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

-- * Domain type implementations

initArtCol :: (WithFile env m) => Id Resource -> m ()
initArtCol colId = init colId articleColInit

articleColInit :: Resource
articleColInit =
  ArticleResource $ ArticleCollection Map.empty Map.empty Map.empty

-- Article

lookupArt :: SealedResource -> Id Article -> Maybe (Entity Article)
lookupArt sRes = lookup articleGetter (unSealResource sRes)

getManyArt
  :: SealedResource -> HashSet (Id Article) -> HashMap (Id Article) Article
getManyArt sRes = getMany articleGetter (unSealResource sRes)

getAllArt :: SealedResource -> HashMap (Id Article) Article
getAllArt = getAll articleGetter . unSealResource

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

articleGetter :: CollectionGetter Article
articleGetter (ArticleResource artCol) = artCollection artCol

articleSetter :: CollectionSetter Article
articleSetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol { artCollection = newVal }

artUpdateTitle :: Id Article -> Text -> StoreEvent
artUpdateTitle aId aTitle =
  updateArt aId $ \oldArt -> oldArt { Article.title = aTitle }

artUpdateState :: Id Article -> ArticleState -> StoreEvent
artUpdateState aId aState =
  updateArt aId $ \oldArt -> oldArt { Article.state = aState }

-- Action

lookupAct :: SealedResource -> Id Action -> Maybe (Entity Action)
lookupAct sRes = lookup actionGetter (unSealResource sRes)

getManyAct
  :: SealedResource -> HashSet (Id Action) -> HashMap (Id Action) Action
getManyAct sRes = getMany actionGetter (unSealResource sRes)

getAllAct :: SealedResource -> HashMap (Id Action) Action
getAllAct = getAll actionGetter . unSealResource

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

actionGetter :: CollectionGetter Action
actionGetter (ArticleResource artCol) = artActCollection artCol

actionSetter :: CollectionSetter Action
actionSetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol { artActCollection = newVal }

-- Capability

lookupCap :: SealedResource -> Id Capability -> Maybe (Entity Capability)
lookupCap sRes = lookup capabilityGetter (unSealResource sRes)

getManyCap
  :: SealedResource
  -> HashSet (Id Capability)
  -> HashMap (Id Capability) Capability
getManyCap sRes = getMany capabilityGetter (unSealResource sRes)

getAllCap :: SealedResource -> HashMap (Id Capability) Capability
getAllCap = getAll capabilityGetter . unSealResource

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

capabilityGetter :: CollectionGetter Capability
capabilityGetter (ArticleResource artCol) = artCapCollection artCol

capabilitySetter :: CollectionSetter Capability
capabilitySetter (ArticleResource artCol) newVal =
  ArticleResource $ artCol { artCapCollection = newVal }

getCapIdForActId
  :: (WithError m) => SealedResource -> Id Action -> m (Id Capability)
getCapIdForActId sRes = asSingleEntry . lookupCapIdForActId sRes

lookupCapIdForActId :: SealedResource -> Id Action -> Maybe (Id Capability)
lookupCapIdForActId sRes actId =
  viaNonEmpty head
    . Map.keys
    . Map.filter ((==) actId . actionId)
    $ capabilityGetter (unSealResource sRes)

-- * Helpers

asSingleEntry :: (WithError m) => Maybe a -> m a
asSingleEntry = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Expected a single entry, but got none"

lookup :: CollectionGetter a -> Resource -> Id a -> Maybe (Entity a)
lookup getter res entId =
  fmap (uncurry Entity . (entId, )) . Map.lookup entId $ getter res

getMany :: CollectionGetter a -> Resource -> HashSet (Id a) -> HashMap (Id a) a
getMany getter res entIds =
  Map.filterWithKey (\key _ -> key `Set.member` entIds) $ getter res

getAll :: CollectionGetter a -> Resource -> HashMap (Id a) a
getAll getter = getter

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

gsetter
  :: CollectionGetter a
  -> CollectionSetter a
  -> (b -> HashMap (Id a) a -> HashMap (Id a) a)
  -> Resource
  -> b
  -> Resource
gsetter fromCol toCol setter oldRes val =
  toCol oldRes . setter val $ fromCol oldRes
