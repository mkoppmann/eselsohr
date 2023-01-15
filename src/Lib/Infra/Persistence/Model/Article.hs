module Lib.Infra.Persistence.Model.Article
    ( ArticlePm
    , fromDomain
    , toDomain
    , migrate
    ) where

import Data.Aeson.Types
    ( FromJSON
    , ToJSON (..)
    , defaultOptions
    , genericToEncoding
    )
import Data.Time.Clock (UTCTime)
import Prelude hiding
    ( id
    , state
    )

import Lib.Domain.Article qualified as Domain
import Lib.Infra.Persistence.Model.Uri qualified as Uri

import Lib.Domain.Article (Article)
import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Id (Id)
import Lib.Infra.Persistence.Model.Id ()
import Lib.Infra.Persistence.Model.Shared (readOrMappingError)

data ArticlePm
    = ArticlePmInit !ArticlePmV1Data
    | ArticlePmV1 !ArticlePmV1Data
    deriving stock (Generic)
    deriving anyclass (FromJSON)

instance ToJSON ArticlePm where
    toEncoding = genericToEncoding defaultOptions

data ArticlePmV1Data = ArticlePmV1Data
    { v1Id :: !(Id Article)
    , v1Title :: !Text
    , v1Uri :: !Text
    , v1State :: !Text
    , v1Creation :: !UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

instance ToJSON ArticlePmV1Data where
    toEncoding = genericToEncoding defaultOptions

------------------------------------------------------------------------
-- Mapper
------------------------------------------------------------------------

fromDomain :: Article -> ArticlePm
fromDomain domArt = do
    let v1Id = domArt.id
        v1Title = toText domArt.title
        v1Uri = Uri.fromDomain domArt.uri
        v1State = show domArt.state
        v1Creation = domArt.creation
    ArticlePmV1 $ ArticlePmV1Data{..}

toDomain :: ArticlePm -> Either AppErrorType Article
toDomain art = case art of
    ArticlePmV1 ArticlePmV1Data{..} -> do
        let domId = v1Id
        domTitle <- Domain.titleFromText v1Title
        domUri <- Uri.toDomain v1Uri
        domState <- readOrMappingError v1State
        pure $ Domain.Article domId domTitle domUri domState v1Creation
    _otherVersion -> toDomain $ migrate art

------------------------------------------------------------------------
-- Migration
------------------------------------------------------------------------

migrate :: ArticlePm -> ArticlePm
migrate = \case
    ArticlePmInit initData -> migrate $ ArticlePmV1 initData
    ArticlePmV1 v1Data -> ArticlePmV1 v1Data
