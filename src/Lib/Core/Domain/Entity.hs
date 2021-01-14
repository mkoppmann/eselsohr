module Lib.Core.Domain.Entity
  ( Entity (..),
  )
where

import Lib.Core.Domain.Id (Id)

data Entity a = Entity
  { id :: !(Id a),
    val :: !a
  }
  deriving stock (Generic, Show, Eq)
