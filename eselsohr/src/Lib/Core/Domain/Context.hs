module Lib.Core.Domain.Context
  ( Context(..)
  ) where

import           Lib.Core.Domain.Accesstoken    ( Reference )
import           Lib.Core.Domain.Capability     ( Action
                                                , Capability
                                                )
import           Lib.Core.Domain.Entity         ( Entity )

data Context = Context
  { ctxRef :: !Reference
  , ctxCap :: !(Entity Capability)
  , ctxAct :: !(Entity Action)
  }
  deriving stock Show
