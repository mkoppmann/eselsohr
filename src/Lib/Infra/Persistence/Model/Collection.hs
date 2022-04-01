module Lib.Infra.Persistence.Model.Collection
  ( CollectionPm
  , mkCollection
  , getArticleList
  , updateArticleList
  , getCapabilityList
  , updateCapabilityList
  , migrate
  ) where

import qualified Data.Map.Strict                                     as Map

import           Data.Aeson.Types                                     ( FromJSON
                                                                      , ToJSON(..)
                                                                      , defaultOptions
                                                                      , genericToEncoding
                                                                      )

import qualified Lib.Infra.Persistence.Model.ArticleList             as ArticleList
import qualified Lib.Infra.Persistence.Model.CapabilityList          as CapabilityList

import           Lib.Infra.Persistence.Model.ArticleList              ( ArticleListPm )
import           Lib.Infra.Persistence.Model.CapabilityList           ( CapabilityListPm )

data CollectionPm
  = CollectionPmInit !CollectionPmV1Data
  | CollectionPmV1 !CollectionPmV1Data
  deriving stock Generic
  deriving anyclass FromJSON

instance ToJSON CollectionPm where
  toEncoding = genericToEncoding defaultOptions

data CollectionPmV1Data = CollectionPmV1Data
  { v1ArticleList    :: !ArticleListPm
  , v1CapabilityList :: !CapabilityListPm
  }
  deriving stock Generic
  deriving anyclass FromJSON

instance ToJSON CollectionPmV1Data where
  toEncoding = genericToEncoding defaultOptions

mkCollection :: CollectionPm
mkCollection = CollectionPmV1 $ CollectionPmV1Data Map.empty Map.empty

getArticleList :: CollectionPm -> ArticleListPm
getArticleList col = case col of
  CollectionPmV1 v1Data -> v1ArticleList v1Data
  _otherVersion         -> getArticleList $ migrate col

updateArticleList :: ArticleListPm -> CollectionPm -> CollectionPm
updateArticleList arts col = case col of
  CollectionPmV1 v1Data -> CollectionPmV1 v1Data { v1ArticleList = arts }
  _otherVersion         -> updateArticleList arts $ migrate col

getCapabilityList :: CollectionPm -> CapabilityListPm
getCapabilityList col = case col of
  CollectionPmV1 v1Data -> v1CapabilityList v1Data
  _otherVersion         -> getCapabilityList $ migrate col

updateCapabilityList :: CapabilityListPm -> CollectionPm -> CollectionPm
updateCapabilityList caps col = case col of
  CollectionPmV1 v1Data -> CollectionPmV1 v1Data { v1CapabilityList = caps }
  _otherVersion         -> updateCapabilityList caps $ migrate col

------------------------------------------------------------------------
-- Migration
------------------------------------------------------------------------

migrate :: CollectionPm -> CollectionPm
migrate = \case
  CollectionPmInit initData -> migrate $ CollectionPmV1 initData
  CollectionPmV1   v1Data   -> CollectionPmV1 $ migrateV1Data v1Data

migrateV1Data :: CollectionPmV1Data -> CollectionPmV1Data
migrateV1Data colData@CollectionPmV1Data {..} = colData { v1ArticleList    = ArticleList.migrate v1ArticleList
                                                        , v1CapabilityList = CapabilityList.migrate v1CapabilityList
                                                        }
