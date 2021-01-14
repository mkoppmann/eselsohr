module Lib.Core.Domain.Uri
  ( Uri (..),
    render,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Binary (Binary)
import qualified Data.Binary as Bin
import qualified Text.URI as U
import Web.HttpApiData (FromHttpApiData (..))

newtype Uri = Uri {unUri :: U.URI}
  deriving (Eq, Show) via U.URI

instance Binary Uri where
  put (Uri uri) =
    Bin.put $ U.render uri

  get =
    either (error . toText . displayException) (pure . Uri) . U.mkURI =<< Bin.get

instance FromHttpApiData Uri where
  parseUrlPiece =
    either (Left . toText . displayException) (Right . Uri) . U.mkURI

instance FromJSON Uri where
  parseJSON =
    withText "Uri" $ either (fail . displayException) (pure . Uri) . U.mkURI

instance ToJSON Uri where
  toJSON (Uri uri) = toJSON $ U.render uri

render :: Uri -> Text
render = toText . U.render . unUri
