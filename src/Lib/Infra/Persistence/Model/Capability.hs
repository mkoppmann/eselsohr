module Lib.Infra.Persistence.Model.Capability
    ( CapabilityPm
    , fromDomain
    , toDomain
    , migrate
    ) where

import Data.Aeson.Types
    ( FromJSON
    , ToJSON (..)
    , defaultOptions
    , genericToEncoding
    )
import Data.Time.Clock (UTCTime)
import Prelude hiding (id)

import qualified Lib.Domain.Capability as Domain

import Lib.Domain.Capability (Capability)
import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Id (Id)
import Lib.Infra.Persistence.Model.Id ()
import Lib.Infra.Persistence.Model.Shared (readOrMappingError)

data CapabilityPm
    = CapabilityPmInit !CapabilityPmV1Data
    | CapabilityPmV1 !CapabilityPmV1Data
    deriving stock (Generic)
    deriving anyclass (FromJSON)

instance ToJSON CapabilityPm where
    toEncoding = genericToEncoding defaultOptions

data CapabilityPmV1Data = CapabilityPmV1Data
    { v1Id :: !(Id Capability)
    , v1ObjectReference :: !Text
    , v1Petname :: !(Maybe Text)
    , v1ExpirationDate :: !(Maybe UTCTime)
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

instance ToJSON CapabilityPmV1Data where
    toEncoding = genericToEncoding defaultOptions

------------------------------------------------------------------------
-- Mapper
------------------------------------------------------------------------

fromDomain :: Capability -> CapabilityPm
fromDomain domCap = do
    let v1Id = domCap.id
        v1ObjectReference = show domCap.objectReference
        v1Petname = domCap.petname
        v1ExpirationDate = domCap.expirationDate
    CapabilityPmV1 $ CapabilityPmV1Data{..}

toDomain :: CapabilityPm -> Either AppErrorType Capability
toDomain cap = case cap of
    CapabilityPmV1 CapabilityPmV1Data{..} -> do
        let domId = v1Id
        domObjectReference <- readOrMappingError v1ObjectReference
        pure $ Domain.Capability domId domObjectReference v1Petname v1ExpirationDate
    _otherVersion -> toDomain $ migrate cap

------------------------------------------------------------------------
-- Migration
------------------------------------------------------------------------

migrate :: CapabilityPm -> CapabilityPm
migrate = \case
    CapabilityPmInit initData -> migrate $ CapabilityPmV1 initData
    CapabilityPmV1 v1Data -> CapabilityPmV1 v1Data
