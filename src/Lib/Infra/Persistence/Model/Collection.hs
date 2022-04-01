module Lib.Infra.Persistence.Model.Collection
  ( CollectionPm(..)
  , mkCollection
  ) where

import qualified Data.Map.Strict                                     as Map

import           Data.Aeson.Types                                     ( FromJSON
                                                                      , ToJSON
                                                                      )

import           Lib.Infra.Persistence.Model.ArticleList              ( ArticleListPm )
import           Lib.Infra.Persistence.Model.CapabilityList           ( CapabilityListPm )

data CollectionPm = CollectionPm
  { articleList    :: !ArticleListPm
  , capabilityList :: !CapabilityListPm
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

mkCollection :: CollectionPm
mkCollection = CollectionPm Map.empty Map.empty
