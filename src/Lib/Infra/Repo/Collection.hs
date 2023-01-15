module Lib.Infra.Repo.Collection
    ( exists
    , createCollection
    ) where

import Data.Map.Strict qualified as Map

import Lib.Infra.Persistence.File qualified as File
import Lib.Infra.Persistence.Model.Capability qualified as CapPm
import Lib.Infra.Persistence.Model.Collection qualified as ColPm

import Lib.Domain.Capability (Capability)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Lib.Infra.Persistence.File (WithFile)

exists :: (WithFile env m) => Id Collection -> m Bool
exists = File.exists

createCollection :: (WithFile env m) => Id Collection -> Id Capability -> Capability -> m ()
createCollection colId capId cap = File.init colId collectionPm
  where
    collectionPm = do
        let newCapList = Map.insert capId (CapPm.fromDomain cap) . ColPm.getCapabilityList $ ColPm.mkCollection
        ColPm.updateCapabilityList newCapList ColPm.mkCollection
