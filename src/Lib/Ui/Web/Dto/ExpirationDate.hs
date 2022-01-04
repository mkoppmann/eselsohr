module Lib.Ui.Web.Dto.ExpirationDate
  ( ExpirationDate(..)
  ) where

import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..) )

newtype ExpirationDate = ExpirationDate {unExpirationDate :: UTCTime}

instance FromHttpApiData ExpirationDate where
  parseUrlPiece = parseExpirationDate

parseExpirationDate :: Text -> Either Text ExpirationDate
parseExpirationDate =
  pure
    .   ExpirationDate
    <=< parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M"
    .   toString
