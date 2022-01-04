module Lib.Domain.Repo.Collection
  ( CollectionRepo(..)
  ) where

import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )

class (Monad m) => CollectionRepo m where
  exists :: Id Collection -> m Bool
  createCollection :: Id Collection -> Id Capability -> Capability -> m ()
