module Lib.Ui.Web.Page.ViewModel.UnlockLink
    ( UnlockLinkVm (..)
    , fromDomain
    ) where

import Lib.Ui.Web.Page.ViewModel.Capability qualified as CapabilityVm

import Lib.Domain.Capability (Capability)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Lib.Ui.Dto.Accesstoken
    ( Accesstoken
    , Reference (..)
    , mkAccesstoken
    )
import Lib.Ui.Web.Page.ViewModel.Capability (CapabilityVm)

data UnlockLinkVm = UnlockLinkVm
    { capVm :: !CapabilityVm
    , acc :: !Accesstoken
    }

fromDomain :: Id Collection -> Id Capability -> Capability -> UnlockLinkVm
fromDomain resId capId cap = do
    let capVm = CapabilityVm.fromDomain cap
        acc = mkAccesstoken $ Reference resId capId
    UnlockLinkVm{..}
