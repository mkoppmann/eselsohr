{- |
Module      : Eselsohr.Web.Server
Description : The WAI application, response headers, body limit, and Warp settings

'application' answers the routes of "Eselsohr.Web.Route" with the fixed pages of "Eselsohr.Web.Html" for a resolved collections directory.
It serves the deletion notice.
Bearer routes pass the presented text to 'bindPresented', which rejects text that is not a well-formed bearer before any file is opened; bearer pages and actions are not served yet, so every bearer route answers like an invalid link, with the same page as a request that matches no route.
HEAD requests route like GET, and Warp sends their headers without a body.

Every response carries 'securityHeaders'.
They disable caching, send no referrer, disable content-type sniffing, ask crawlers not to index or follow the page, and set a content security policy.
The policy allows no resources by default, so pages load no scripts, styles, images, or frames.
It also forbids a base URL, allows form submissions only to the same origin, and forbids framing the page, because @default-src@ does not cover these three directives.

A request that declares a body longer than 'bodyLimit' receives the too-large page before routing.
'readLimitedBody' reads a body of undeclared length and gives up beyond the limit, for routes that read forms.

'serve' runs 'application' with 'serverSettings' for a configuration and a resolved collections directory, and reports the listener and public origin to the operator once Warp has bound the listener.
'serverSettings' configures Warp for a t'Listener' and an operator-message sink.
Warp answers malformed requests and uncaught exceptions itself, outside the application, so its exception-response hook renders the error pages with the same headers.
Its exception hook writes only a fixed message with the exception's type, never the exception's text or the request, and skips the exceptions that Warp's default hook skips, such as malformed requests and closed connections.

> serve config directory (Text.hPutStrLn stderr)
-}
module Eselsohr.Web.Server
    ( -- * Application
      application

      -- * Response headers
    , securityHeaders
    , withSecurityHeaders

      -- * Body limit
    , bodyLimit
    , readLimitedBody

      -- * Running the server
    , serve
    , serverSettings
    , exceptionResponse
    ) where

import Control.Exception (SomeAsyncException, SomeException (..), fromException, throw)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (typeOf)
import Eselsohr.Config (Config (..), Listener (..), renderPublicOrigin)
import Eselsohr.Store.Runtime (CollectionDirectory, bindPresented)
import Eselsohr.Web.Html (Page (..), renderPage)
import Eselsohr.Web.Route (Route (..), route)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

------------------------------------------------------------------------
-- Application
------------------------------------------------------------------------

application :: CollectionDirectory -> Application
application directory = withSecurityHeaders $ \request respond ->
    respond $
        if declaredLength request > Just bodyLimit
            then pageResponse status413 BodyTooLargePage
            else case route (requestMethod request) (pathInfo request) of
                Just DeletionNotice -> pageResponse status200 DeletionNoticePage
                Just (BearerPage presented) -> bearerRoute presented
                Just (BearerAction presented _) -> bearerRoute presented
                Nothing -> invalidLink
  where
    invalidLink = pageResponse status404 InvalidLinkPage
    bearerRoute presented = case bindPresented directory presented of
        Nothing -> invalidLink
        Just _ -> invalidLink
    declaredLength request = case requestBodyLength request of
        KnownLength size -> Just (fromIntegral size)
        ChunkedBody -> Nothing

-- | A fixed page with its content type and length, without the security headers.
pageResponse :: Status -> Page -> Response
pageResponse status page = responseLBS status headers body
  where
    body = renderPage page
    headers =
        [ (hContentType, "text/html; charset=utf-8")
        , (hContentLength, Char8.pack (show (LazyByteString.length body)))
        ]

------------------------------------------------------------------------
-- Response headers
------------------------------------------------------------------------

securityHeaders :: ResponseHeaders
securityHeaders =
    [ ("Cache-Control", "no-store")
    , ("Referrer-Policy", "no-referrer")
    , ("X-Content-Type-Options", "nosniff")
    , ("X-Robots-Tag", "noindex, nofollow")
    , ("Content-Security-Policy", "default-src 'none'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'")
    ]

-- | Add the security headers to every response, replacing headers of the same names.
withSecurityHeaders :: Middleware
withSecurityHeaders app request respond = app request (respond . addSecurityHeaders)

addSecurityHeaders :: Response -> Response
addSecurityHeaders = mapResponseHeaders $ \headers ->
    securityHeaders <> filter ((`notElem` map fst securityHeaders) . fst) headers

------------------------------------------------------------------------
-- Body limit
------------------------------------------------------------------------

-- | 160 KiB, which admits a title and URL at their input limits in four-byte characters, each percent-encoded.
bodyLimit :: Int
bodyLimit = 160 * 1024

-- | Read the whole request body, or 'Nothing' as soon as it exceeds 'bodyLimit'.
readLimitedBody :: Request -> IO (Maybe ByteString)
readLimitedBody request = go 0 []
  where
    go size chunks = getRequestBodyChunk request >>= next size chunks
    next size chunks chunk
        | ByteString.null chunk = pure (Just (ByteString.concat (reverse chunks)))
        | size' > bodyLimit = pure Nothing
        | otherwise = go size' (chunk : chunks)
      where
        size' = size + ByteString.length chunk

------------------------------------------------------------------------
-- Running the server
------------------------------------------------------------------------

-- | Serve until the process ends; failing to resolve or bind the listener throws an 'IOError'.
serve :: Config -> CollectionDirectory -> (Text -> IO ()) -> IO ()
serve config directory logMessage = runSettings (setBeforeMainLoop (logMessage started) settings) (application directory)
  where
    listener = configListener config
    settings = serverSettings listener logMessage
    started = "Eselsohr listens on " <> address listener <> " for " <> renderPublicOrigin (configPublicOrigin config) <> "."
    address (Listener host port)
        | Text.any (== ':') host = "[" <> host <> "]:" <> Text.pack (show port)
        | otherwise = host <> ":" <> Text.pack (show port)

serverSettings :: Listener -> (Text -> IO ()) -> Settings
serverSettings listener logMessage =
    setHost (fromString (Text.unpack (listenerHost listener)))
        . setPort (listenerPort listener)
        . setOnException (const (logException logMessage))
        . setOnExceptionResponse exceptionResponse
        $ defaultSettings

-- | Report the exception's type only, because its text could contain request data.
logException :: (Text -> IO ()) -> SomeException -> IO ()
logException logMessage exception@(SomeException inner) =
    when (defaultShouldDisplayException exception) $
        logMessage ("A request failed with an exception of type " <> Text.pack (show (typeOf inner)) <> ".")

-- | The error page for an exception that Warp caught, with the security headers.
exceptionResponse :: SomeException -> Response
exceptionResponse exception
    | Just (_ :: SomeAsyncException) <- fromException exception = throw exception
    | otherwise = addSecurityHeaders $ case fromException exception of
        Just PayloadTooLarge -> pageResponse status413 BodyTooLargePage
        Just RequestHeaderFieldsTooLarge -> pageResponse status431 BadRequestPage
        Just _ -> pageResponse status400 BadRequestPage
        Nothing -> pageResponse status500 ServerErrorPage
