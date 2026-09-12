{- |
Module      : Eselsohr.WebUrl
Description : Validated HTTP and HTTPS bookmark URLs

A t'WebUrl' is an absolute HTTP or HTTPS bookmark URL with a nonempty host, no user information, and an omitted or default port.
'parseWebUrl' validates the syntax and input length, normalizes the scheme and host, removes explicit default ports, and encodes Unicode text for rendering.
'renderWebUrl' returns the normalized URL.

Validation performs no DNS lookup or network access.
Local and private hosts are accepted, so a stored bookmark still needs destination checks before fetching.
-}
module Eselsohr.WebUrl
    ( WebUrl
    , WebUrlError (..)
    , parseWebUrl
    , renderWebUrl
    ) where

import Data.ByteString qualified as ByteString
import Data.Char (isAscii, isControl, isSpace, ord, toLower)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import Network.URI qualified as URI
import Numeric (readHex)
import Text.IDNA2008 qualified as IDNA

-- | A syntactically validated bookmark URL, not a safe network destination.
newtype WebUrl = WebUrl Text
    deriving stock (Eq)

data WebUrlError
    = UrlTooLong
    | InvalidUrlSyntax
    | UnsupportedScheme
    | InvalidHost
    | UserInfoNotAllowed
    | NonstandardPort
    deriving stock (Eq, Show)

parseWebUrl :: Text -> Either WebUrlError WebUrl
parseWebUrl input
    | Text.compareLength input 8192 == GT = Left UrlTooLong
    | Text.any forbiddenCharacter input = Left InvalidUrlSyntax
    | otherwise = do
        uri <-
            maybe (Left InvalidUrlSyntax) Right . URI.parseURI $
                URI.escapeURIString isAscii (Text.unpack input)
        let scheme = map toLower (URI.uriScheme uri)
        defaultPort <- case scheme of
            "http:" -> Right "80"
            "https:" -> Right "443"
            _ -> Left UnsupportedScheme

        auth <- maybe (Left InvalidHost) Right (URI.uriAuthority uri)
        if null (URI.uriUserInfo auth) then Right () else Left UserInfoNotAllowed
        let port = URI.uriPort auth
        if null port || dropWhile (== '0') (drop 1 port) == defaultPort
            then Right ()
            else Left NonstandardPort

        host <- normalizeHost (URI.uriRegName auth)
        let normalized =
                uri
                    { URI.uriScheme = scheme
                    , URI.uriAuthority = Just auth{URI.uriRegName = host, URI.uriPort = ""}
                    }
            rendered = Text.pack (URI.uriToString id normalized "")

        if Text.compareLength rendered 8192 == GT
            then Left UrlTooLong
            else Right (WebUrl rendered)
  where
    forbiddenCharacter c =
        isControl c || isSpace c || c `elem` ['\\', '\x2028', '\x2029']

normalizeHost :: String -> Either WebUrlError String
normalizeHost host
    | null host = Left InvalidHost
    | Text.isPrefixOf "[" (Text.pack host) =
        if URI.isIPv6address . Text.unpack . Text.dropEnd 1 . Text.drop 1 $ Text.pack host
            then Right (map toLower host)
            else Left InvalidHost
    | otherwise = do
        decoded <- decodeHost host
        -- The IDNA library also accepts DNS presentation escapes, which URLs do not use.
        if Text.any (== '\\') decoded then Left InvalidHost else Right ()
        (domain, _) <-
            either (const (Left InvalidHost)) Right $
                IDNA.parseDomainOpts IDNA.idnLabelForms flags decoded
        if null (IDNA.toLabels domain) then Left InvalidHost else Right ()

        let ascii = IDNA.domainToAscii domain
            trailingDot = if Text.isSuffixOf "." decoded then "." else ""
        Right (Text.unpack (ascii <> trailingDot))
  where
    flags = IDNA.defaultIdnaFlags <> IDNA.MAPCASE <> IDNA.MAPNFC

decodeHost :: String -> Either WebUrlError Text
decodeHost host = do
    bytes <- decode host
    either (const (Left InvalidHost)) Right . Text.decodeUtf8' $ ByteString.pack bytes
  where
    decode :: String -> Either WebUrlError [Word8]
    decode [] = Right []
    decode ('%' : a : b : rest) = case readHex [a, b] of
        [(byte, "")] -> (byte :) <$> decode rest
        _ -> Left InvalidHost
    decode ('%' : _) = Left InvalidHost
    decode (c : rest)
        | isAscii c = (fromIntegral (ord c) :) <$> decode rest
        | otherwise = Left InvalidHost

renderWebUrl :: WebUrl -> Text
renderWebUrl (WebUrl url) = url
