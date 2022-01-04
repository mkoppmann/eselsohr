module Lib.Infra.Persistence.Model.Collection
  ( CollectionPm(..)
  , mkCollection
  ) where

import qualified Data.Map.Strict               as Map

import           Codec.Serialise.Class          ( Serialise )

import           Lib.Infra.Persistence.Model.ArticleList
                                                ( ArticleListPm )
import           Lib.Infra.Persistence.Model.CapabilityList
                                                ( CapabilityListPm )

data CollectionPm = CollectionPm
  { articleList    :: !ArticleListPm
  , capabilityList :: !CapabilityListPm
  }
  deriving stock Generic
  deriving anyclass Serialise

mkCollection :: CollectionPm
mkCollection = CollectionPm Map.empty Map.empty
