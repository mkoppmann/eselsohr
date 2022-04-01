module Lib.Infra.Persistence.Model.Capability
  ( CapabilityPm(..)
  , fromDomain
  , toDomain
  ) where

import           Data.Aeson.Types                                     ( FromJSON
                                                                      , ToJSON
                                                                      )
import           Data.Time.Clock                                      ( UTCTime )
import           Prelude                                       hiding ( id )

import qualified Lib.Domain.Capability                               as Domain

import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Persistence.Model.Id                       ( )
import           Lib.Infra.Persistence.Model.Shared                   ( readOrMappingError )

data CapabilityPm = CapabilityPm
  { id              :: !(Id Capability)
  , objectReference :: !Text
  , petname         :: !(Maybe Text)
  , expirationDate  :: !(Maybe UTCTime)
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Eq CapabilityPm where
  (==) a b = id a == id b

instance Ord CapabilityPm where
  compare a b = compare (expirationDate a) (expirationDate b)

fromDomain :: Capability -> CapabilityPm
fromDomain domCap = do
  let id              = Domain.id domCap
      objectReference = show $ Domain.objectReference domCap
      petname         = Domain.petname domCap
      expirationDate  = Domain.expirationDate domCap
  CapabilityPm { .. }

toDomain :: CapabilityPm -> Either AppErrorType Capability
toDomain CapabilityPm {..} = do
  let domId = id
  domObjectReference <- readOrMappingError objectReference
  pure $ Domain.Capability domId domObjectReference petname expirationDate
