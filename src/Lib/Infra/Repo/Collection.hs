module Lib.Infra.Repo.Collection
  ( createCollection
  ) where

import qualified Data.Map.Strict               as Map

import qualified Lib.Infra.Persistence.File    as File
import qualified Lib.Infra.Persistence.Model.Capability
                                               as CapPm
import qualified Lib.Infra.Persistence.Model.Collection
                                               as ColPm

import           Lib.Domain.Capability          ( Capability )
import           Lib.Domain.Collection          ( Collection )
import           Lib.Domain.Id                  ( Id )
import           Lib.Infra.Persistence.File     ( WithFile )
import           Lib.Infra.Persistence.Model.Capability
                                                ( CapabilityPm )
import           Lib.Infra.Persistence.Model.Collection
                                                ( CollectionPm )

createCollection
  :: (WithFile env m) => Id Collection -> Id Capability -> Capability -> m ()
createCollection colId capId cap = File.init colId collectionPm
 where
  collectionPm = addCapability capId (CapPm.fromDomain cap) ColPm.mkCollection

addCapability :: Id Capability -> CapabilityPm -> CollectionPm -> CollectionPm
addCapability capPmId capPm colPm = newColPm
 where
  oldCapList = ColPm.capabilityList colPm
  newCapList = Map.insert capPmId capPm oldCapList
  newColPm   = colPm { ColPm.capabilityList = newCapList }
