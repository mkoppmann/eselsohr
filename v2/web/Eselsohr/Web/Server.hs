{- |
Module      : Eselsohr.Web.Server
Description : The WAI application, response headers, body limit, and Warp settings

'application' answers the routes of "Eselsohr.Web.Route" for a resolved collections directory and an operator-message sink.
It serves the deletion notice, the page of each bearer, and the add form's submissions.
Bearer routes pass the presented text to 'bindPresented', which rejects text that is not a well-formed bearer before any file is opened.
A bearer page runs 'showPage' with the grant and article readers bound to the presented bearer and renders it with "Eselsohr.Web.Page"; the @added@ query flag adds the fixed submission notice, and other query parameters are ignored.

An addition reads the body within 'bodyLimit', decodes the @url@ and @title@ fields, and validates them before any Store call.
Valid input runs 'submitArticle' with the article writer and redirects with 303 to @/b/\<bearer\>?added@, the same for a new and an already stored URL.
Invalid input shows the page again with 422, the submitted values, and a message for each failed field, but only to a grant that may add; any other grant receives the invalid-link page, as a denied addition does.
An undecodable body gives 400, and a body over the limit gives 413.
The other bearer actions are not served yet, so they answer like an invalid link, with the same page as a request that matches no route.

'pageResult', 'rejectedResult', and 'submitResult' decide the responses to outcomes without effects.
A denial gives the invalid-link page; a busy collection gives 503 with @Retry-After: 1@; corrupt or unavailable storage gives the error page with 500 and an operator message naming the fault and the collection identifier, never the bearer.
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
    , pageResult
    , rejectedResult
    , submitResult

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
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (typeOf)
import Effectful (runEff)
import Eselsohr.Config (Config (..), Listener (..), renderPublicOrigin)
import Eselsohr.Id (CollectionId, renderCollectionId)
import Eselsohr.Store (StoreFault (..))
import Eselsohr.Store.Runtime (Binding, CollectionDirectory, bindPresented, bindingCollection, runArticleReader, runArticleWriter, runGrantReader)
import Eselsohr.Web.Form (AddInput (..), decodeAddForm, validateAdd)
import Eselsohr.Web.Handler (PageOutcome (..), SubmitOutcome (..), showPage, submitArticle)
import Eselsohr.Web.Html (Page (..), renderPage)
import Eselsohr.Web.Page (PageExtras (..), mayAddArticles, plainPage, renderBearerPage)
import Eselsohr.Web.Route (Action (..), Route (..), route)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

------------------------------------------------------------------------
-- Application
------------------------------------------------------------------------

application :: (Text -> IO ()) -> CollectionDirectory -> Application
application logMessage directory = withSecurityHeaders $ \request respond -> do
    response <-
        if declaredLength request > Just bodyLimit
            then pure (pageResponse status413 BodyTooLargePage)
            else case route (requestMethod request) (pathInfo request) of
                Just DeletionNotice -> pure (pageResponse status200 DeletionNoticePage)
                Just (BearerPage presented) -> withBinding presented (servePage logMessage presented (extrasFor request))
                Just (BearerAction presented AddArticle) -> withBinding presented (serveAddition logMessage presented request)
                Just (BearerAction presented _) -> withBinding presented (const (pure invalidLink))
                Nothing -> pure invalidLink
    respond response
  where
    withBinding presented use = maybe (pure invalidLink) use (bindPresented directory presented)
    declaredLength request = case requestBodyLength request of
        KnownLength size -> Just (fromIntegral size)
        ChunkedBody -> Nothing

invalidLink :: Response
invalidLink = pageResponse status404 InvalidLinkPage

-- | The submission notice appears when the query names the @added@ flag.
extrasFor :: Request -> PageExtras
extrasFor request = plainPage{extrasSubmitted = any ((== "added") . fst) (queryString request)}

-- | Run the page handler with readers bound to the bearer, then log and respond as 'pageResult' decides.
servePage :: (Text -> IO ()) -> Text -> PageExtras -> Binding -> IO Response
servePage logMessage presented extras binding = do
    outcome <- runEff . runArticleReader binding . runGrantReader binding $ showPage
    respondWith logMessage (pageResult (bindingCollection binding) presented extras outcome)

-- | Read and validate the add form, then submit it or show the page again with its problems.
serveAddition :: (Text -> IO ()) -> Text -> Request -> Binding -> IO Response
serveAddition logMessage presented request binding = do
    body <- readLimitedBody request
    case body of
        Nothing -> pure (pageResponse status413 BodyTooLargePage)
        Just bytes -> case decodeAddForm bytes of
            Nothing -> pure (pageResponse status400 BadRequestPage)
            Just (urlText, titleText) -> case validateAdd urlText titleText of
                Left problems -> do
                    outcome <- runEff . runArticleReader binding . runGrantReader binding $ showPage
                    respondWith logMessage (rejectedResult collection presented (AddInput urlText titleText problems) outcome)
                Right (url, title) -> do
                    outcome <- runEff . runArticleWriter binding $ submitArticle url title
                    respondWith logMessage (submitResult collection presented outcome)
  where
    collection = bindingCollection binding

respondWith :: (Text -> IO ()) -> (Maybe Text, Response) -> IO Response
respondWith logMessage (message, response) = mapM_ logMessage message >> pure response

-- | The operator message, if any, and the response for a page outcome.
pageResult :: CollectionId -> Text -> PageExtras -> PageOutcome -> (Maybe Text, Response)
pageResult collection presented extras = \case
    PageShown view articles -> (Nothing, htmlResponse status200 (renderBearerPage presented extras view articles))
    PageDenied -> (Nothing, invalidLink)
    PageFault fault -> faultResult collection fault

-- | The page with invalid input for a grant that may add, or the invalid-link page for any other grant.
rejectedResult :: CollectionId -> Text -> AddInput -> PageOutcome -> (Maybe Text, Response)
rejectedResult collection presented input = \case
    PageShown view articles
        | mayAddArticles view -> (Nothing, htmlResponse status422 (renderBearerPage presented plainPage{extrasInput = input} view articles))
        | otherwise -> (Nothing, invalidLink)
    outcome -> pageResult collection presented plainPage outcome

-- | The redirect to the page with the submission notice, or the response to a denial or fault.
submitResult :: CollectionId -> Text -> SubmitOutcome -> (Maybe Text, Response)
submitResult collection presented = \case
    Submitted -> (Nothing, responseLBS status303 [(hLocation, encodeUtf8 ("/b/" <> presented <> "?added")), (hContentLength, "0")] "")
    SubmitDenied -> (Nothing, invalidLink)
    SubmitFault fault -> faultResult collection fault

faultResult :: CollectionId -> StoreFault -> (Maybe Text, Response)
faultResult collection = \case
    Busy -> (Nothing, mapResponseHeaders (("Retry-After", "1") :) (pageResponse status503 BusyPage))
    Corrupt -> (Just (faultMessage "has corrupt data"), pageResponse status500 ServerErrorPage)
    Unavailable -> (Just (faultMessage "is unavailable"), pageResponse status500 ServerErrorPage)
  where
    faultMessage problem = "Collection " <> renderCollectionId collection <> " " <> problem <> "."

-- | A fixed page with its content type and length, without the security headers.
pageResponse :: Status -> Page -> Response
pageResponse status = htmlResponse status . renderPage

htmlResponse :: Status -> LazyByteString.LazyByteString -> Response
htmlResponse status body = responseLBS status headers body
  where
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
serve config directory logMessage = runSettings (setBeforeMainLoop (logMessage started) settings) (application logMessage directory)
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
