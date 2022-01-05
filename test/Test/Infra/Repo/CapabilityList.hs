module Test.Infra.Repo.CapabilityList
  ( testSaveAll
  ) where

import qualified Lib.Infra.Persistence.Model.CapabilityList          as CapListPm
import qualified Lib.Infra.Persistence.Model.Collection              as ColPm
import qualified Lib.Infra.Repo.CapabilityList                       as CapListRepo

import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.CapabilityList                       ( CapabilityListAction )
import           Lib.Infra.Error                                      ( WithError
                                                                      , throwOnError
                                                                      )
import           Test.App.Env                                         ( CollectionState
                                                                      , Has
                                                                      , grab
                                                                      )

testSaveAll
  :: (MonadReader env m, Has CollectionState env, MonadIO m, WithError m)
  => Id Collection
  -> Seq CapabilityListAction
  -> m ()
testSaveAll _colId updates = do
  collectionRef   <- grab @CollectionState
  collection      <- readIORef collectionRef
  capabilities    <- throwOnError . CapListPm.toDomain $ ColPm.capabilityList collection
  newCapabilities <- fmap CapListPm.fromDomain . throwOnError $ foldlM CapListRepo.apply capabilities updates
  writeIORef collectionRef $ collection { ColPm.capabilityList = newCapabilities }
