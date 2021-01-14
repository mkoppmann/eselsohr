module Lib.Core.Domain.ExpirationDate
  ( ExpirationDate,
    unExpirationDate,
    mkExpirationDate,
    toExpirationDate,
    expDateToText,
  )
where

import Data.Binary (Binary)
import Data.Binary.Instances.Time ()
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Web.HttpApiData (FromHttpApiData (..))

newtype ExpirationDate = ExpirationDate {unExpirationDate :: UTCTime}
  deriving (Binary, Eq, Ord, Show) via UTCTime

instance FromHttpApiData ExpirationDate where
  parseUrlPiece = mkExpirationDate

mkExpirationDate :: Text -> Either Text ExpirationDate
mkExpirationDate =
  pure
    . ExpirationDate
    <=< parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" . toString

toExpirationDate :: UTCTime -> ExpirationDate
toExpirationDate = ExpirationDate

expDateToText :: ExpirationDate -> Text
expDateToText =
  Prelude.toText
    . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"
    . unExpirationDate
