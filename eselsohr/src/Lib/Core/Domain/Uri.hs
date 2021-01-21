module Lib.Core.Domain.Uri
  ( Uri,
    unUri,
    mkUri,
    render,
    baseUri,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Binary (Binary)
import qualified Data.Binary as Bin
import Lens.Micro ((^.), (^?), _Right)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Net.IPv6.Helper as IPv6
import qualified Text.Show
import Text.URI (URI)
import qualified Text.URI as U
import qualified Text.URI.Lens as UL
import Validation (Validation (..), failure, validateAll, validationToEither)
import Web.HttpApiData (FromHttpApiData (..))

newtype Uri = Uri {unUri :: U.URI}
  deriving (Eq, Show) via U.URI

instance Binary Uri where
  get = either (fail . toString) pure . mkUri =<< Bin.get
  put (Uri uri) = Bin.put $ U.render uri

instance FromHttpApiData Uri where
  parseUrlPiece = either Left pure . mkUri

instance FromJSON Uri where
  parseJSON = withText "Uri" $ either (fail . toString) pure . mkUri

instance ToJSON Uri where
  toJSON (Uri uri) = toJSON $ U.render uri

-- | Type representing different validation errors.
data UriValidationError
  = -- | Only port 80 and 443 are allowed.
    ForbiddenPort
  | -- | Only HTTP and HTTPS are allowed.
    ForbiddenProtocol
  | -- | Hostnames like @localhost@ are forbidden.
    ForbiddenHostname
  | -- | Only public IPv4 ranges are allowed.
    ForbiddenIPv4Range
  | -- | Only public IPv6 ranges are allowed.
    ForbiddenIPv6Range

instance Show UriValidationError where
  show = \case
    ForbiddenPort -> "Only port 80 and 443 are allowed."
    ForbiddenProtocol -> "Only HTTP and HTTPS are allowed."
    ForbiddenHostname -> "Hostnames like `localhost` are forbidden."
    ForbiddenIPv4Range -> "Only public IPv4 ranges are allowed."
    ForbiddenIPv6Range -> "Only public IPv6 ranges are allowed."

mkUri :: Text -> Either Text Uri
mkUri url = case U.mkURI url of
  Left err -> Left . toText $ displayException err
  Right uri -> validationToEither . bimap show Uri $ validateUri uri
  where
    validateUri :: URI -> Validation (NonEmpty UriValidationError) URI
    validateUri =
      validateAll
        [ validatePort,
          validateProtocol,
          validateHostname,
          validateIPv4,
          validateIPv6
        ]

    validatePort :: URI -> Validation (NonEmpty UriValidationError) URI
    validatePort uri =
      case join $ uri ^? UL.uriAuthority . _Right . UL.authPort of
        Just 80 -> Success uri
        Just 443 -> Success uri
        Nothing -> Success uri
        _nonAllowedPort -> failure ForbiddenPort

    validateProtocol :: URI -> Validation (NonEmpty UriValidationError) URI
    validateProtocol uri = case U.unRText <$> uri ^. UL.uriScheme of
      Just "http" -> Success uri
      Just "https" -> Success uri
      Nothing -> Success uri
      _nonAllowedProtocol -> failure ForbiddenProtocol

    validateHostname :: URI -> Validation (NonEmpty UriValidationError) URI
    validateHostname uri = case getHostname uri of
      Just "localhost" -> failure ForbiddenHostname
      _otherHostnames -> Success uri

    validateIPv4 :: URI -> Validation (NonEmpty UriValidationError) URI
    validateIPv4 uri = case IPv4.public <$> (IPv4.decode =<< getHostname uri) of
      Nothing -> Success uri
      Just isPub -> if isPub then Success uri else failure ForbiddenIPv4Range

    validateIPv6 :: URI -> Validation (NonEmpty UriValidationError) URI
    validateIPv6 uri = case IPv6.public <$> (IPv6.decode =<< getHostname uri) of
      Nothing -> Success uri
      Just isPub -> if isPub then Success uri else failure ForbiddenIPv6Range

    getHostname :: URI -> Maybe Text
    getHostname uri =
      U.unRText <$> uri ^? UL.uriAuthority . _Right . UL.authHost

render :: Uri -> Text
render = toText . U.render . unUri

baseUri :: Text -> Uri
baseUri url = case U.mkURI url of
  Left err -> error . (<>) "Invalid base url: " . toText $ displayException err
  Right uri -> Uri uri
