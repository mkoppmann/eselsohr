module Lib.Ui.Web.Page.ViewModel.Capability
  ( CapabilityVm(..)
  , fromDomain
  ) where

import           Data.Time.Clock                ( UTCTime )
import           Prelude                 hiding ( id )

import qualified Lib.Domain.Capability         as Capability

import           Lib.Domain.Capability          ( Capability )
import           Lib.Domain.Id                  ( Id )

data CapabilityVm = CapabilityVm
  { id             :: !(Id Capability)
  , petname        :: !(Maybe Text)
  , expirationDate :: !(Maybe UTCTime)
  }

fromDomain :: Capability -> CapabilityVm
fromDomain cap = do
  let id             = Capability.id cap
      petname        = Capability.petname cap
      expirationDate = Capability.expirationDate cap
  CapabilityVm { .. }
