module Lib.Infra.Persistence.Model.CapabilityList
    ( CapabilityListPm
    , fromDomain
    , toDomain
    , migrate
    ) where

import Data.Map.Strict qualified as Map

import Lib.Domain.CapabilityList qualified as Domain
import Lib.Infra.Persistence.Model.Capability qualified as Capability

import Lib.Domain.Capability (Capability)
import Lib.Domain.CapabilityList (CapabilityList)
import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Id (Id)
import Lib.Infra.Persistence.Model.Capability (CapabilityPm)
import Lib.Infra.Persistence.Model.Shared
    ( modelListFromDomain
    , modelListToDomain
    )

type CapabilityListPm = Map (Id Capability) CapabilityPm

------------------------------------------------------------------------
-- Mapper
------------------------------------------------------------------------

fromDomain :: CapabilityList -> CapabilityListPm
fromDomain = Map.fromList . modelListFromDomain Capability.fromDomain . Domain.toMap

toDomain :: CapabilityListPm -> Either AppErrorType CapabilityList
toDomain = Right . Domain.fromMap . Map.fromList <=< modelListToDomain Capability.toDomain

------------------------------------------------------------------------
-- Migration
------------------------------------------------------------------------

migrate :: CapabilityListPm -> CapabilityListPm
migrate = fmap Capability.migrate
