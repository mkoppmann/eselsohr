module Lib.Infra.Repo.Collection
  ( exists
  , createCollection
  ) where

import qualified Data.Map.Strict                                     as Map

import qualified Lib.Infra.Persistence.File                          as File
import qualified Lib.Infra.Persistence.Model.Capability              as CapPm
import qualified Lib.Infra.Persistence.Model.Collection              as ColPm

import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Persistence.File                           ( WithFile )

exists :: (WithFile env m) => Id Collection -> m Bool
exists = File.exists

createCollection :: (WithFile env m) => Id Collection -> Id Capability -> Capability -> m ()
createCollection colId capId cap = File.init colId collectionPm
 where
  collectionPm = do
    let newCapList = Map.insert capId (CapPm.fromDomain cap) . ColPm.capabilityList $ ColPm.mkCollection
    ColPm.mkCollection { ColPm.capabilityList = newCapList }
