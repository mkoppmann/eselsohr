module Lib.Infra.Persistence.Model.CapabilityList
  ( CapabilityListPm
  , fromDomain
  , toDomain
  ) where

import qualified Data.Map.Strict               as Map

import qualified Lib.Domain.CapabilityList     as Domain
import qualified Lib.Infra.Persistence.Model.Capability
                                               as Capability

import           Lib.Domain.Capability          ( Capability )
import           Lib.Domain.CapabilityList      ( CapabilityList )
import           Lib.Domain.Error               ( AppErrorType )
import           Lib.Domain.Id                  ( Id )
import           Lib.Infra.Persistence.Model.Capability
                                                ( CapabilityPm )
import           Lib.Infra.Persistence.Model.Shared
                                                ( modelListFromDomain
                                                , modelListToDomain
                                                )

type CapabilityListPm = Map (Id Capability) CapabilityPm

fromDomain :: CapabilityList -> CapabilityListPm
fromDomain =
  Map.fromList . modelListFromDomain Capability.fromDomain . Domain.toMap

toDomain :: CapabilityListPm -> Either AppErrorType CapabilityList
toDomain =
  Right
    .   Domain.fromMap
    .   Map.fromList
    <=< modelListToDomain Capability.toDomain
