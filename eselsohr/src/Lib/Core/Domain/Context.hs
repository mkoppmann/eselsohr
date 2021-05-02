module Lib.Core.Domain.Context
  ( Context(..)
  ) where

import           Lib.Core.Domain.Capability     ( Action
                                                , Capability
                                                )
import           Lib.Core.Domain.Entity         ( Entity )
import           Lib.Core.Domain.Id             ( Id )
import           Lib.Core.Domain.Resource       ( Resource )

data Context = Context
  { ctxResId :: !(Id Resource)
  , ctxCap   :: !(Entity Capability)
  , ctxAct   :: !(Entity Action)
  }
  deriving stock Show
