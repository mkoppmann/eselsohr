module Lib.Core.Domain.Context
  ( Context(..)
  ) where

import           Lib.Core.Domain.Capability     ( ObjectReference )
import           Lib.Core.Domain.Id             ( Id )
import           Lib.Core.Domain.Resource       ( Resource )

data Context = Context
  { ctxResId  :: !(Id Resource)
  , ctxObjRef :: !ObjectReference
  }
  deriving stock Show
