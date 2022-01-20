module Lib.Infra.Repo.CapabilityList
  ( loadAll
  , nextId
  , saveAll
  ) where

import qualified Lib.App.Port                                        as Port
import qualified Lib.Domain.Repo.CapabilityList                      as Repo
import qualified Lib.Infra.Persistence.File                          as File
import qualified Lib.Infra.Persistence.Model.CapabilityList          as CapListPm
import qualified Lib.Infra.Persistence.Model.Collection              as ColPm

import           Lib.App.Port                                         ( MonadRandom )
import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.CapabilityList                            ( CapabilityList )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.CapabilityList                       ( CapabilityListAction )
import           Lib.Infra.Error                                      ( WithError
                                                                      , throwOnError
                                                                      )
import           Lib.Infra.Persistence.File                           ( WithFile )
import           Lib.Infra.Persistence.Model.Collection               ( CollectionPm )
import           Lib.Infra.Persistence.Queue                          ( WithQueue
                                                                      , commit
                                                                      )

loadAll :: (WithError m, WithFile env m) => Id Collection -> m CapabilityList
loadAll colId = throwOnError . CapListPm.toDomain =<< File.load colId ColPm.capabilityList

nextId :: (MonadRandom m) => m (Id Capability)
nextId = Port.getRandomId

saveAll :: (WithError m, WithFile env m, WithQueue env m) => Id Collection -> Seq CapabilityListAction -> m ()
saveAll colId updates = do
  let action = File.save update colId
  commit colId action
 where
  update :: WithError m => CollectionPm -> m CollectionPm
  update colPm = do
    capList    <- throwOnError . CapListPm.toDomain $ ColPm.capabilityList colPm
    newCapList <- throwOnError $ foldlM Repo.apply capList updates
    pure $ colPm { ColPm.capabilityList = CapListPm.fromDomain newCapList }
