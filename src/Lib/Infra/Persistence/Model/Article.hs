module Lib.Infra.Persistence.Model.Article
  ( ArticlePm(..)
  , fromDomain
  , toDomain
  ) where

import           Codec.Serialise.Class                                ( Serialise )
import           Codec.Serialise.UUID                                 ( )
import           Data.Time.Clock                                      ( UTCTime )
import           Prelude                                       hiding ( id
                                                                      , state
                                                                      )

import qualified Lib.Domain.Article                                  as Domain
import qualified Lib.Infra.Persistence.Model.Uri                     as Uri

import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Infra.Persistence.Model.Id                       ( )
import           Lib.Infra.Persistence.Model.Shared                   ( readOrMappingError )

data ArticlePm = ArticlePm
  { id       :: !(Id Article)
  , title    :: !Text
  , uri      :: !Text
  , state    :: !Text
  , creation :: !UTCTime
  }
  deriving stock Generic
  deriving anyclass Serialise

fromDomain :: Article -> ArticlePm
fromDomain domArt = do
  let id       = Domain.id domArt
      title    = toText $ Domain.title domArt
      uri      = Uri.fromDomain $ Domain.uri domArt
      state    = show $ Domain.state domArt
      creation = Domain.creation domArt
  ArticlePm { .. }

toDomain :: ArticlePm -> Either AppErrorType Article
toDomain ArticlePm {..} = do
  let domId = id
  domTitle <- Domain.titleFromText title
  domUri   <- Uri.toDomain uri
  domState <- readOrMappingError state
  pure $ Domain.Article domId domTitle domUri domState creation
