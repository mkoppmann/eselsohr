module Lib.Domain.Repo.Collection
  ( CollectionRepo(..)
  ) where

import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )

class (Monad m) => CollectionRepo m where
  createCollection :: Id Collection -> Id Capability -> Capability -> m ()
