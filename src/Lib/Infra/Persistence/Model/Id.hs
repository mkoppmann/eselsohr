{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Infra.Persistence.Model.Id where

import           Codec.Serialise.Class          ( Serialise )
import           Codec.Serialise.UUID           ( )
import           Data.UUID                      ( UUID )

import qualified Lib.Domain.Id                 as Domain
import           Lib.Domain.Id                  ( Id )

deriving via UUID instance Serialise (Id a)
