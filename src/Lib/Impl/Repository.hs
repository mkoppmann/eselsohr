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
  , getOneArt
  , lookupArt
  , getAllArt
  , insertArt
  , updateArtTitle
  , updateArtState
  , deleteArt
  , capabilityGetter
  , getOneCap
  , lookupCap
  , getAllCap
  , insertCap
  , updateCap
  , deleteCap

  -- * Helpers
  , asSingleEntry
  ) where

import qualified Data.Map.Strict               as Map
import           Lib.App                        ( AppErrorType
                                                , WithError
                                                , WriteQueue
                                                , grab
                                                , storeError
                                                , throwOnNothing
                                                )
import           Lib.Core.Domain                ( Article(..)
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

type CollectionGetter a = (Resource -> Map (Id a) a)

type CollectionSetter a = (Resource -> Map (Id a) a -> Resource)

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
articleColInit = Resource Map.empty Map.empty

-- Article

getOneArt
  :: (WithError m) => SealedResource -> Id Article -> m (Entity Article)
getOneArt sRes = asSingleEntry . lookupArt sRes

lookupArt :: SealedResource -> Id Article -> Maybe (Entity Article)
lookupArt sRes = lookup articleGetter (unSealResource sRes)

getAllArt :: SealedResource -> Map (Id Article) Article
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
articleGetter Resource {..} = artCollection

articleSetter :: CollectionSetter Article
articleSetter res newVal = res { artCollection = newVal }

updateArtTitle :: Id Article -> Text -> StoreEvent
updateArtTitle aId aTitle =
  updateArt aId $ \oldArt -> oldArt { Article.title = aTitle }

updateArtState :: Id Article -> ArticleState -> StoreEvent
updateArtState aId aState =
  updateArt aId $ \oldArt -> oldArt { Article.state = aState }

-- Capability

getOneCap
  :: (WithError m) => SealedResource -> Id Capability -> m (Entity Capability)
getOneCap sRes = asSingleEntry . lookupCap sRes

lookupCap :: SealedResource -> Id Capability -> Maybe (Entity Capability)
lookupCap sRes = lookup capabilityGetter (unSealResource sRes)

getAllCap :: SealedResource -> Map (Id Capability) Capability
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
capabilityGetter Resource {..} = capCollection

capabilitySetter :: CollectionSetter Capability
capabilitySetter res newVal = res { capCollection = newVal }

-- * Helpers

asSingleEntry :: (WithError m) => Maybe a -> m a
asSingleEntry = withFrozenCallStack . throwOnNothing singleEntryError

singleEntryError :: AppErrorType
singleEntryError = storeError "Expected a single entry, but got none"

lookup :: CollectionGetter a -> Resource -> Id a -> Maybe (Entity a)
lookup getter res entId =
  fmap (uncurry Entity . (entId, )) . Map.lookup entId $ getter res

getAll :: CollectionGetter a -> Resource -> Map (Id a) a
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
  where setter' _ = Map.adjust updater entId

deleteSetter
  :: CollectionGetter a -> CollectionSetter a -> Resource -> Id a -> Resource
deleteSetter getter setter = gsetter getter setter Map.delete

gsetter
  :: CollectionGetter a
  -> CollectionSetter a
  -> (b -> Map (Id a) a -> Map (Id a) a)
  -> Resource
  -> b
  -> Resource
gsetter fromCol toCol setter oldRes val =
  toCol oldRes . setter val $ fromCol oldRes
