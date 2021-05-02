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
  , Impl.getCapIdForActId
  , Impl.lookupCapIdForActId

  -- * 'Article' helpers
  , Impl.artUpdateTitle
  , Impl.artUpdateState
  ) where

import           Lib.App                        ( App
                                                , WithError
                                                )
import           Lib.Core.Domain                ( Action(..)
                                                , Article
                                                , Capability(..)
                                                , Context
                                                , Entity(..)
                                                , Id
                                                , Resource
                                                , StoreEvent
                                                )
import           Lib.Impl.Repository            ( SealedResource )
import qualified Lib.Impl.Repository           as Impl

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

class ReadResource a where
  lookup :: SealedResource -> Id a -> Maybe (Entity a)
  getMany :: SealedResource -> HashSet (Id a) -> HashMap (Id a) a
  getAll :: SealedResource -> HashMap (Id a) a

class WriteResource a where
  insert :: Id a -> a -> StoreEvent
  update :: Id a -> (a -> a) -> StoreEvent
  delete :: Id a -> StoreEvent

instance ReadState App where
  load = Impl.load

instance WriteState App where
  init   = Impl.initArtCol
  commit = Impl.commit

instance ReadResource Article where
  lookup  = Impl.lookupArt
  getMany = Impl.getManyArt
  getAll  = Impl.getAllArt

instance WriteResource Article where
  insert = Impl.insertArt
  update = Impl.updateArt
  delete = Impl.deleteArt

instance ReadResource Capability  where
  lookup  = Impl.lookupCap
  getMany = Impl.getManyCap
  getAll  = Impl.getAllCap

instance WriteResource Capability  where
  insert = Impl.insertCap
  update = Impl.updateCap
  delete = Impl.deleteCap

instance ReadResource Action  where
  lookup  = Impl.lookupAct
  getMany = Impl.getManyAct
  getAll  = Impl.getAllAct

instance WriteResource Action where
  insert = Impl.insertAct
  update = Impl.updateAct
  delete = Impl.deleteAct

-- Helpers

getOne
  :: (WithError m, ReadResource a) => SealedResource -> Id a -> m (Entity a)
getOne sRes = Impl.asSingleEntry . lookup sRes
