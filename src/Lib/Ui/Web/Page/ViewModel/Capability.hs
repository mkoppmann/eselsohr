module Lib.Ui.Web.Page.ViewModel.Capability
    ( CapabilityVm (..)
    , fromDomain
    ) where

import Data.Time.Clock (UTCTime)
import Prelude hiding (id)

import Lib.Domain.Capability qualified as Capability

import Lib.Domain.Capability (Capability)
import Lib.Domain.Id (Id)

data CapabilityVm = CapabilityVm
    { id :: !(Id Capability)
    , petname :: !(Maybe Text)
    , expirationDate :: !(Maybe UTCTime)
    }

fromDomain :: Capability -> CapabilityVm
fromDomain cap = do
    let id = cap.id
        petname = cap.petname
        expirationDate = cap.expirationDate
    CapabilityVm{..}
