module Test.Infra.Repo.Collection
    ( testExists
    , testCreateCollection
    ) where

import Data.Map.Strict qualified as Map

import Lib.Infra.Persistence.Model.Capability qualified as CapPm
import Lib.Infra.Persistence.Model.Collection qualified as ColPm

import Lib.Domain.Capability (Capability)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Test.App.Env
    ( CollectionState
    , Has
    , grab
    )

testExists :: (Monad m) => Id Collection -> m Bool
testExists _colId = pure False

testCreateCollection
    :: (MonadReader env m, Has CollectionState env, MonadIO m) => Id Collection -> Id Capability -> Capability -> m ()
testCreateCollection _colId capId cap = flip writeIORef collectionPm =<< grab @CollectionState
  where
    collectionPm = do
        let capabilities = Map.insert capId (CapPm.fromDomain cap) . ColPm.getCapabilityList $ ColPm.mkCollection
        ColPm.updateCapabilityList capabilities ColPm.mkCollection
