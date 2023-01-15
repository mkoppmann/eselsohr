module Lib.Infra.Persistence.Cleanup
    ( removeExpiredCapabilitiesFromCollections
    ) where

import Lib.App.Command qualified as Command
import Lib.Domain.Capability qualified as Cap
import Lib.Infra.Persistence.File qualified as File

import Lib.App.Port (MonadTime)
import Lib.Domain.Capability (ObjectReference)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Lib.Domain.Repo.CapabilityList (CapabilityListRepo)
import Lib.Infra.Error
    ( WithError
    , throwOnErrorM
    )
import Lib.Infra.Persistence.File (WithFile)

removeExpiredCapabilitiesFromCollections :: (CapabilityListRepo m, MonadTime m, WithError m, WithFile env m) => m ()
removeExpiredCapabilitiesFromCollections = do
    colIds <- File.allIdsInPath
    traverse_ (removeExpiredCapabilitiesFromCollection objRef) colIds
  where
    objRef = Cap.OverviewRef $ Cap.mkOverviewPerms False False True False

removeExpiredCapabilitiesFromCollection
    :: (CapabilityListRepo m, MonadTime m, WithError m) => ObjectReference -> Id Collection -> m ()
removeExpiredCapabilitiesFromCollection objRef colId = throwOnErrorM $ Command.removeExpiredCapabilities command
  where
    command = Command.RemoveExpiredCapabilities{..}
