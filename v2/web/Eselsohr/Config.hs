{- |
Module      : Eselsohr.Config
Description : Validated configuration from environment variables

A t'Config' holds the data directory, the listener address, and the public origin that Eselsohr reads from three environment variables:

* @ESELSOHR_DATA_DIR@ names the data directory and is required.
* @ESELSOHR_LISTEN@ names the listener as @host:port@ and defaults to @127.0.0.1:6979@.
* @ESELSOHR_PUBLIC_ORIGIN@ names the origin that printed URLs start with and is required.

'parseConfig' validates the values supplied by a lookup function, so it reads no environment itself.
A value that is set but malformed is an error, never a reason to use the default.
'loadConfig' reads the variables through an effectful lookup, such as 'System.Environment.lookupEnv', parses them, and then checks that the data directory and its @collections@ directory exist.
It changes no environment variable and creates no directory.

A listener host is a name, a dotted IPv4 address, or an IPv6 address in brackets, and its port lies between 1 and 65535.
Names are not resolved here.

A t'PublicOrigin' is the configured text exactly: a lowercase @http@ or @https@ scheme, a lowercase ASCII host name or IP literal, and an optional port.
It has no trailing slash, path, query, fragment, or user information, and it is not normalized.
Plain @http@ is accepted only for the loopback hosts @localhost@, @127.0.0.0/8@, and @[::1]@.

> parseConfig (`lookup` [("ESELSOHR_DATA_DIR", "/srv/eselsohr"), ("ESELSOHR_PUBLIC_ORIGIN", "http://localhost:6979")])

'renderConfigError' names the variable and the expected form; errors carry no configured text.
-}
module Eselsohr.Config
    ( -- * Configuration
      Config (..)
    , parseConfig
    , loadConfig
    , collectionsDirectory

      -- * Listener
    , Listener (..)
    , parseListener

      -- * Public origin
    , PublicOrigin
    , parsePublicOrigin
    , renderPublicOrigin

      -- * Errors
    , Variable (..)
    , ConfigError (..)
    , variableName
    , renderConfigError
    ) where

import Control.Monad (guard, unless)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (isIPv4address, isIPv6address)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

data Config = Config
    { configDataDirectory :: FilePath
    , configListener :: Listener
    , configPublicOrigin :: PublicOrigin
    }
    deriving stock (Eq, Show)

-- | Validate the values that the lookup function returns for the three variables.
parseConfig :: (String -> Maybe String) -> Either ConfigError Config
parseConfig lookupVariable = do
    dataDirectory <- required DataDirectoryVariable >>= parseDataDirectory
    listener <- maybe (Right defaultListener) (parseListener . Text.pack) (value ListenVariable)
    origin <- required PublicOriginVariable >>= parsePublicOrigin . Text.pack
    pure (Config dataDirectory listener origin)
  where
    value = lookupVariable . Text.unpack . variableName
    required variable = maybe (Left (MissingValue variable)) Right (value variable)

parseDataDirectory :: String -> Either ConfigError FilePath
parseDataDirectory path
    | null path = Left (MalformedValue DataDirectoryVariable)
    | otherwise = Right path

-- | Read and parse the variables, then require the data and collections directories.
loadConfig :: (String -> IO (Maybe String)) -> IO (Either ConfigError Config)
loadConfig lookupVariable = do
    let names = map (Text.unpack . variableName) [DataDirectoryVariable, ListenVariable, PublicOriginVariable]
    found <- catMaybes <$> traverse (\name -> fmap (name,) <$> lookupVariable name) names
    case parseConfig (`lookup` found) of
        Left err -> pure (Left err)
        Right config -> checkDirectories config

checkDirectories :: Config -> IO (Either ConfigError Config)
checkDirectories config = do
    dataDirectory <- doesDirectoryExist (configDataDirectory config)
    collections <- doesDirectoryExist (collectionsDirectory config)
    pure $ case (dataDirectory, collections) of
        (False, _) -> Left MissingDataDirectory
        (True, False) -> Left MissingCollectionsDirectory
        (True, True) -> Right config

-- | The directory holding the collection files, inside the data directory.
collectionsDirectory :: Config -> FilePath
collectionsDirectory config = configDataDirectory config </> "collections"

------------------------------------------------------------------------
-- Listener
------------------------------------------------------------------------

-- | A host name or IP address without brackets, and a port.
data Listener = Listener
    { listenerHost :: Text
    , listenerPort :: Int
    }
    deriving stock (Eq, Show)

defaultListener :: Listener
defaultListener = Listener "127.0.0.1" 6979

parseListener :: Text -> Either ConfigError Listener
parseListener input = maybe (Left (MalformedValue ListenVariable)) Right $ do
    (host, rest) <- splitHost input
    guard (isIPv6Host host || isIPv4 host || isHostName (Text.toLower host))
    port <- Text.stripPrefix ":" rest >>= parsePort
    pure (Listener (unbracket host) port)
  where
    unbracket host = fromMaybe host (Text.stripPrefix "[" host >>= Text.stripSuffix "]")

------------------------------------------------------------------------
-- Public origin
------------------------------------------------------------------------

-- | The configured origin text, which printed URLs start with.
newtype PublicOrigin = PublicOrigin Text
    deriving stock (Eq, Show)

parsePublicOrigin :: Text -> Either ConfigError PublicOrigin
parsePublicOrigin input = do
    (secure, authority) <- maybe malformed Right (stripScheme input)
    (host, rest) <- maybe malformed Right (splitHost authority)
    unless (isLowerIPv6Host host || isIPv4 host || isHostName host) malformed
    unless (Text.null rest || isJust (Text.stripPrefix ":" rest >>= parsePort)) malformed
    unless (secure || isLoopback host) (Left InsecurePublicOrigin)
    pure (PublicOrigin input)
  where
    malformed = Left (MalformedValue PublicOriginVariable)
    stripScheme text = case Text.stripPrefix "https://" text of
        Just authority -> Just (True, authority)
        Nothing -> (False,) <$> Text.stripPrefix "http://" text
    isLowerIPv6Host host = isIPv6Host host && not (Text.any isAsciiUpper host)
    isLoopback host = host == "localhost" || host == "[::1]" || (isIPv4 host && Text.isPrefixOf "127." host)

renderPublicOrigin :: PublicOrigin -> Text
renderPublicOrigin (PublicOrigin origin) = origin

------------------------------------------------------------------------
-- Hosts and ports
------------------------------------------------------------------------

-- | Split a bracketed IPv6 host or a host without colons from the rest of an authority.
splitHost :: Text -> Maybe (Text, Text)
splitHost authority
    | Text.isPrefixOf "[" authority = do
        let (address, rest) = Text.breakOn "]" authority
        closing <- Text.stripPrefix "]" rest
        pure (address <> "]", closing)
    | otherwise = Just (Text.break (== ':') authority)

isIPv6Host :: Text -> Bool
isIPv6Host host = case Text.stripPrefix "[" host >>= Text.stripSuffix "]" of
    Just address -> isIPv6address (Text.unpack address)
    Nothing -> False

isIPv4 :: Text -> Bool
isIPv4 = isIPv4address . Text.unpack

-- | A DNS name in lowercase ASCII whose last label is not numeric, so that it cannot be mistaken for an address.
isHostName :: Text -> Bool
isHostName host =
    not (Text.null host)
        && Text.length host <= 253
        && all validLabel (Text.splitOn "." host)
        && not (Text.all isDigit (Text.takeWhileEnd (/= '.') host))
  where
    validLabel label =
        not (Text.null label)
            && Text.length label <= 63
            && Text.all (\c -> isAsciiLower c || isDigit c || c == '-') label
            && not (Text.isPrefixOf "-" label || Text.isSuffixOf "-" label)

-- | A port from 1 to 65535, written without a sign or leading zeros.
parsePort :: Text -> Maybe Int
parsePort text = do
    guard (not (Text.null text) && Text.length text <= 5 && Text.all isDigit text && not (Text.isPrefixOf "0" text))
    port <- readMaybe (Text.unpack text)
    guard (port <= 65535)
    pure port

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

data Variable = DataDirectoryVariable | ListenVariable | PublicOriginVariable
    deriving stock (Eq, Show)

data ConfigError
    = MissingValue Variable
    | MalformedValue Variable
    | InsecurePublicOrigin
    | MissingDataDirectory
    | MissingCollectionsDirectory
    deriving stock (Eq, Show)

variableName :: Variable -> Text
variableName = \case
    DataDirectoryVariable -> "ESELSOHR_DATA_DIR"
    ListenVariable -> "ESELSOHR_LISTEN"
    PublicOriginVariable -> "ESELSOHR_PUBLIC_ORIGIN"

-- | An operator message naming the variable and its expected form.
renderConfigError :: ConfigError -> Text
renderConfigError = \case
    MissingValue variable -> variableName variable <> " is not set."
    MalformedValue DataDirectoryVariable -> "ESELSOHR_DATA_DIR must not be empty."
    MalformedValue ListenVariable -> "ESELSOHR_LISTEN must be host:port, such as 127.0.0.1:6979 or [::1]:6979."
    MalformedValue PublicOriginVariable ->
        "ESELSOHR_PUBLIC_ORIGIN must be a lowercase origin such as https://example.org, without a trailing slash, path, query, fragment, or user information."
    InsecurePublicOrigin -> "ESELSOHR_PUBLIC_ORIGIN must use https unless its host is localhost, 127.0.0.0/8, or [::1]."
    MissingDataDirectory -> "ESELSOHR_DATA_DIR must name an existing directory."
    MissingCollectionsDirectory -> "ESELSOHR_DATA_DIR must contain an existing collections directory."
