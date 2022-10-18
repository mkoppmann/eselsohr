module Lib.Domain.Uri
  ( Uri(..)
  , UriValidationError(..)
  , mkUri
  , unfilteredUri
  , getHostname
  , baseUri
  ) where

import qualified Data.Text                                           as T
import qualified Net.IPv4                                            as IPv4
import qualified Net.IPv6                                            as IPv6
import qualified Net.IPv6.Helper                                     as IPv6
import qualified Text.Show
import qualified Text.URI                                            as U
import qualified Text.URI.Lens                                       as UL
import qualified Validation

import           Lens.Micro                                           ( (^.)
                                                                      , (^?)
                                                                      , _Right
                                                                      )
import           Text.URI                                             ( URI )
import           Validation                                           ( Validation
                                                                      , failure
                                                                      , validateAll
                                                                      , validationToEither
                                                                      )

import           Lib.Domain.Error                                     ( AppErrorType
                                                                      , invalid
                                                                      )

newtype Uri = Uri {unUri :: U.URI}
  deriving (Eq, Show) via U.URI

instance ToText Uri where
  toText = toText . U.render . coerce

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
  show ForbiddenPort      = "Only port 80 and 443 are allowed."
  show ForbiddenProtocol  = "Only HTTP and HTTPS are allowed."
  show ForbiddenHostname  = "Hostnames like `localhost` are forbidden."
  show ForbiddenIPv4Range = "Only public IPv4 ranges are allowed."
  show ForbiddenIPv6Range = "Only public IPv6 ranges are allowed."

mkUri :: Text -> Either AppErrorType Uri
mkUri url = case U.mkURI url of
  Left  err -> Left . invalid . toText $ displayException err
  Right uri -> validationToEither . bimap (invalid . show) Uri $ validateUri uri
 where
  validateUri :: URI -> Validation (NonEmpty UriValidationError) URI
  validateUri = validateAll [validatePort, validateProtocol, validateHostname, validateIPv4, validateIPv6]

  validatePort :: URI -> Validation (NonEmpty UriValidationError) URI
  validatePort uri = case join $ uri ^? UL.uriAuthority . _Right . UL.authPort of
    Just 80         -> Validation.Success uri
    Just 443        -> Validation.Success uri
    Nothing         -> Validation.Success uri
    _nonAllowedPort -> failure ForbiddenPort

  validateProtocol :: URI -> Validation (NonEmpty UriValidationError) URI
  validateProtocol uri = case U.unRText <$> uri ^. UL.uriScheme of
    Just "http"         -> Validation.Success uri
    Just "https"        -> Validation.Success uri
    Nothing             -> Validation.Success uri
    _nonAllowedProtocol -> failure ForbiddenProtocol

  validateHostname :: URI -> Validation (NonEmpty UriValidationError) URI
  validateHostname uri = case getHostname' uri of
    Just "localhost" -> failure ForbiddenHostname
    _otherHostnames  -> Validation.Success uri

  validateIPv4 :: URI -> Validation (NonEmpty UriValidationError) URI
  validateIPv4 uri = case IPv4.public <$> (IPv4.decode =<< getHostname' uri) of
    Nothing    -> Validation.Success uri
    Just isPub -> if isPub then Validation.Success uri else failure ForbiddenIPv4Range

  validateIPv6 :: URI -> Validation (NonEmpty UriValidationError) URI
  validateIPv6 uri = case IPv6.public <$> (IPv6.decode =<< getHostnameFromIpv6 uri) of
    Nothing    -> Validation.Success uri
    Just isPub -> if isPub then Validation.Success uri else failure ForbiddenIPv6Range

  getHostnameFromIpv6 :: URI -> Maybe Text
  getHostnameFromIpv6 = fmap dropIPv6Brackets . getHostname'

  {- | Literal IPv6 addresses are put into brackets in URLs:
  https://www.ietf.org/rfc/rfc2732.txt
  -}
  dropIPv6Brackets :: Text -> Text
  dropIPv6Brackets = T.dropEnd 1 . T.drop 1

{- | Returns an 'Uri' like 'mkUri' does but with no applied validation. Use
with caution.
-}
unfilteredUri :: Text -> Either AppErrorType Uri
unfilteredUri = either failureCase uri . U.mkURI
 where
  failureCase :: SomeException -> Either AppErrorType a
  failureCase = Left . invalid . toText . displayException

  uri :: URI -> Either AppErrorType Uri
  uri = pure . Uri

getHostname :: Uri -> Maybe Text
getHostname (Uri uri) = getHostname' uri

baseUri :: Text -> Uri
baseUri url = case U.mkURI url of
  Left  err -> error . (<>) "Invalid base url: " . toText $ displayException err
  Right uri -> Uri uri

getHostname' :: URI -> Maybe Text
getHostname' uri = U.unRText <$> uri ^? UL.uriAuthority . _Right . UL.authHost
