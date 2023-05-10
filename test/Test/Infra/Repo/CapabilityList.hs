module Test.Infra.Repo.CapabilityList
    ( testLoadAll
    , testSaveAll
    ) where

import qualified Lib.Infra.Persistence.Model.CapabilityList as CapListPm
import qualified Lib.Infra.Persistence.Model.Collection as ColPm

import Lib.Domain.CapabilityList (CapabilityList)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Lib.Domain.Repo.CapabilityList (CapabilityListAction)
import Lib.Infra.Error
    ( WithError
    , throwOnError
    )
import Lib.Infra.Persistence.Model.Collection (CollectionPm)
import Test.App.Env
    ( CollectionState
    , Has
    , grab
    )

testLoadAll
    :: (MonadReader env m, Has CollectionState env, MonadIO m, WithError m) => Id Collection -> m CapabilityList
testLoadAll _colId = capabilitiesFromCollection =<< readIORef =<< grab @CollectionState

testSaveAll
    :: (MonadReader env m, Has CollectionState env, MonadIO m, WithError m)
    => Id Collection
    -> Seq CapabilityListAction
    -> m ()
testSaveAll _colId updates = do
    collectionRef <- grab @CollectionState
    collection <- readIORef collectionRef
    capabilities <- capabilitiesFromCollection collection
    newCapabilities <- fmap CapListPm.fromDomain . throwOnError $ foldlM (&) capabilities updates
    writeIORef collectionRef $ ColPm.updateCapabilityList newCapabilities collection

capabilitiesFromCollection :: (WithError m) => CollectionPm -> m CapabilityList
capabilitiesFromCollection = throwOnError . CapListPm.toDomain . ColPm.getCapabilityList
