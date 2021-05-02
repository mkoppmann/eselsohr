module Lib.Core.Effect.Repository
  (
  -- * Access to a 'Resource'.
    SealedResource
  , ContextState(..)
  , ReadState(..)
  , ReadResource(..)
  , WriteState(..)
  , WriteResource(..)
  , RWState

  -- * Getter helpers
  , getOne
  , getCapIdForActId
  , lookupCapIdForActId

  -- * 'Article' helpers
  , artUpdateTitle
  , artUpdateState
  ) where

import           Lib.App                        ( App
                                                , WithError
                                                )
import           Lib.Core.Domain                ( Action(..)
                                                , Article
                                                , ArticleState
                                                , Capability(..)
                                                , Context
                                                , Entity(..)
                                                , Id
                                                , Resource
                                                , StoreEvent
                                                )
import qualified Lib.Core.Domain.Article       as Article
import qualified Lib.Impl.Repository           as Impl

newtype SealedResource = SealedResource {unSealResource :: Resource}

data ContextState = ContextState
  { csContext  :: !Context
  , csResource :: !SealedResource
  }

class (Monad m) => ReadState m where
  load :: Id Resource -> m SealedResource

class (Monad m) => WriteState m where
  init :: Id Resource -> m ()
  commit :: Id Resource -> Seq StoreEvent -> m ()

type RWState m = (ReadState m, WriteState m)

instance ReadState App where
  load = pure . SealedResource <=< Impl.load

instance WriteState App where
  init colId = Impl.init colId Impl.articleColInit
  commit = Impl.commit

class ReadResource a where
  lookup :: SealedResource -> Id a -> Maybe (Entity a)
  getMany :: SealedResource -> HashSet (Id a) -> HashMap (Id a) a
  getAll :: SealedResource -> HashMap (Id a) a

class WriteResource a where
  insert :: Id a -> a -> StoreEvent
  update :: Id a -> (a -> a) -> StoreEvent
  delete :: Id a -> StoreEvent

instance ReadResource Article where
  lookup  = lookupArt
  getMany = getManyArt
  getAll  = getAllArt

instance WriteResource Article where
  insert = Impl.insertArt
  update = Impl.updateArt
  delete = Impl.deleteArt

instance ReadResource Capability  where
  lookup  = lookupCap
  getMany = getManyCap
  getAll  = getAllCap

instance WriteResource Capability  where
  insert = Impl.insertCap
  update = Impl.updateCap
  delete = Impl.deleteCap

instance ReadResource Action  where
  lookup  = lookupAct
  getMany = getManyAct
  getAll  = getAllAct

instance WriteResource Action where
  insert = Impl.insertAct
  update = Impl.updateAct
  delete = Impl.deleteAct

getOne
  :: (WithError m, ReadResource a) => SealedResource -> Id a -> m (Entity a)
getOne sRes = Impl.asSingleEntry . lookup sRes

getAllCap :: SealedResource -> HashMap (Id Capability) Capability
getAllCap = Impl.getAll Impl.capabilityGetter . unSealResource

getManyCap
  :: SealedResource
  -> HashSet (Id Capability)
  -> HashMap (Id Capability) Capability
getManyCap sRes = Impl.getMany Impl.capabilityGetter (unSealResource sRes)

lookupCap :: SealedResource -> Id Capability -> Maybe (Entity Capability)
lookupCap sRes = Impl.lookup Impl.capabilityGetter (unSealResource sRes)

getAllAct :: SealedResource -> HashMap (Id Action) Action
getAllAct = Impl.getAll Impl.actionGetter . unSealResource

getManyAct
  :: SealedResource -> HashSet (Id Action) -> HashMap (Id Action) Action
getManyAct sRes = Impl.getMany Impl.actionGetter (unSealResource sRes)

lookupAct :: SealedResource -> Id Action -> Maybe (Entity Action)
lookupAct sRes = Impl.lookup Impl.actionGetter (unSealResource sRes)

getCapIdForActId
  :: (WithError m) => SealedResource -> Id Action -> m (Id Capability)
getCapIdForActId sRes = Impl.asSingleEntry . lookupCapIdForActId sRes

lookupCapIdForActId :: SealedResource -> Id Action -> Maybe (Id Capability)
lookupCapIdForActId sRes = Impl.getCapIdForActId (unSealResource sRes)

getAllArt :: SealedResource -> HashMap (Id Article) Article
getAllArt = Impl.getAll Impl.articleGetter . unSealResource

getManyArt
  :: SealedResource -> HashSet (Id Article) -> HashMap (Id Article) Article
getManyArt sRes = Impl.getMany Impl.articleGetter (unSealResource sRes)

lookupArt :: SealedResource -> Id Article -> Maybe (Entity Article)
lookupArt sRes = Impl.lookup Impl.articleGetter (unSealResource sRes)

-- Helper

artUpdateTitle :: Id Article -> Text -> StoreEvent
artUpdateTitle aId aTitle =
  Impl.updateArt aId $ \oldArt -> oldArt { Article.title = aTitle }

artUpdateState :: Id Article -> ArticleState -> StoreEvent
artUpdateState aId aState =
  Impl.updateArt aId $ \oldArt -> oldArt { Article.state = aState }
