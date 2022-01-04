module Lib.Infra.Repo.CapabilityList
  ( nextId
  , saveAll
  ) where

import qualified Lib.App.Port                  as Port
import qualified Lib.Domain.CapabilityList     as CapList
import qualified Lib.Domain.Repo.CapabilityList
                                               as CapListRepo
import qualified Lib.Infra.Persistence.File    as File
import qualified Lib.Infra.Persistence.Model.CapabilityList
                                               as CapListPm
import qualified Lib.Infra.Persistence.Model.Collection
                                               as ColPm

import           Lib.App.Port                   ( MonadRandom )
import           Lib.Domain.Capability          ( Capability )
import           Lib.Domain.CapabilityList      ( CapabilityList )
import           Lib.Domain.Collection          ( Collection )
import           Lib.Domain.Error               ( AppErrorType )
import           Lib.Domain.Id                  ( Id )
import           Lib.Domain.Repo.CapabilityList ( CapabilityListAction )
import           Lib.Infra.Error                ( WithError
                                                , throwOnError
                                                )
import           Lib.Infra.Persistence.File     ( WithFile )
import           Lib.Infra.Persistence.Model.Collection
                                                ( CollectionPm )
import           Lib.Infra.Persistence.Queue    ( WithQueue
                                                , commit
                                                )

nextId :: (MonadRandom m) => m (Id Capability)
nextId = Port.getRandomId

saveAll
  :: (WithError m, WithFile env m, WithQueue env m)
  => Id Collection
  -> Seq CapabilityListAction
  -> m ()
saveAll colId updates = do
  let action = File.save update colId
  commit colId action
 where
  update :: WithError m => CollectionPm -> m CollectionPm
  update colPm = do
    capList    <- throwOnError . CapListPm.toDomain $ ColPm.capabilityList colPm
    newCapList <- throwOnError $ foldlM apply capList updates
    pure $ colPm { ColPm.capabilityList = CapListPm.fromDomain newCapList }

apply
  :: CapabilityList
  -> CapabilityListAction
  -> Either AppErrorType CapabilityList
apply capList = \case
  CapListRepo.AddUnlockLink perm capId cap ->
    CapList.addUnlockLink perm capId cap capList
  CapListRepo.RemoveUnlockLink perm capId ->
    pure $ CapList.removeUnlockLink perm capId capList
  CapListRepo.AddShareUnlockLinks perm capId cap ->
    CapList.addShareUnlockLinks perm capId cap capList
  CapListRepo.RemoveShareUnlockLinks perm capId ->
    pure $ CapList.removeShareUnlockLinks perm capId capList
  CapListRepo.AddShareArticleList perm capId cap ->
    CapList.addShareArticleList perm capId cap capList
  CapListRepo.RemoveShareArticleList perm capId ->
    pure $ CapList.removeShareArticleList perm capId capList
  CapListRepo.AddShareArticle perm capId cap ->
    CapList.addShareArticle perm capId cap capList
  CapListRepo.RemoveShareArticle perm capId ->
    pure $ CapList.removeShareArticle perm capId capList
