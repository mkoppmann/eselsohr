{- |
Module      : ServerTests
Description : HTTP tests of the application and the Warp settings

Most tests start the application under Warp on an ephemeral port for the duration of the test and talk to it with raw requests; the body limit is also checked by calling the application directly.
The tests check the security headers on page, error, and Warp-generated responses, HEAD without a body, the declared and undeclared body limit, and that operator messages never contain request data.
Warp closes the connection without a reply when it cannot split the request line, so that case is checked for the absence of a reply and of a log.
-}
module ServerTests (serverTests) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Eselsohr.Config (Listener (..))
import Eselsohr.Web.Server
import Network.HTTP.Types (statusCode)
import Network.Socket (PortNumber)
import Network.Wai (Application, RequestBodyLength (..), defaultRequest, pathInfo, requestBodyLength, responseStatus, setRequestBodyChunks)
import Network.Wai.Handler.Warp (testWithApplicationSettings, withApplicationSettings)
import Network.Wai.Internal (ResponseReceived (..))
import RawHttp
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

serverTests :: TestTree
serverTests = testGroup "Server" [pageTests, limitTests, warpTests]

pageTests :: TestTree
pageTests =
    testGroup
        "Pages"
        [ testCase "the deletion notice" . withServer $ \send -> do
            response <- send (request "GET" "/deleted" [] "")
            rawStatus response @?= 200
            assertPageHeaders response
        , testCase "an unknown path is an invalid link" . withServer $ \send -> do
            response <- send (request "GET" "/collections" [] "")
            rawStatus response @?= 404
            assertPageHeaders response
        , testCase "bearer routes answer like unknown paths" . withServer $ \send -> do
            unknown <- send (request "GET" "/nothing" [] "")
            responses <-
                mapM
                    send
                    [ request "GET" ("/b/" <> bearerShaped) [] ""
                    , request "POST" ("/b/" <> bearerShaped <> "/add") ["Content-Length: 0"] ""
                    , request "GET" ("/b/" <> bearerShaped <> "/") [] ""
                    , request "PUT" "/deleted" ["Content-Length: 0"] ""
                    ]
            mapM_ (\response -> (rawStatus response, rawBody response) @?= (404, rawBody unknown)) responses
            mapM_ (\response -> header "Location" response @?= []) responses
        , testCase "HEAD has the headers of GET and no body" . withServer $ \send -> do
            get <- send (request "GET" "/deleted" [] "")
            head' <- send (request "HEAD" "/deleted" [] "")
            rawStatus head' @?= 200
            assertPageHeaders head'
            header "Content-Length" head' @?= header "Content-Length" get
            rawBody head' @?= ""
        ]

limitTests :: TestTree
limitTests =
    testGroup
        "Body limit"
        [ testCase "a declared body over the limit is rejected before routing" . withServer $ \send -> do
            response <- send (request "POST" ("/b/" <> bearerShaped <> "/add") ["Content-Length: " <> Char8.pack (show (bodyLimit + 1))] "")
            rawStatus response @?= 413
            assertPageHeaders response
        , testCase "a declared body at the limit is routed, one byte more is not" $ do
            statusFor (declaring bodyLimit) >>= (@?= Just 200)
            statusFor (declaring (bodyLimit + 1)) >>= (@?= Just 413)
        , testCase "reading undeclared bodies below, at, and above the limit" $ do
            readChunks [ByteString.replicate 1000 97, ByteString.replicate 1000 98] >>= (@?= Just (ByteString.replicate 1000 97 <> ByteString.replicate 1000 98))
            readChunks (replicate 160 (ByteString.replicate 1024 97)) >>= (@?= Just (ByteString.replicate bodyLimit 97))
            readChunks (replicate 160 (ByteString.replicate 1024 97) <> ["a"]) >>= (@?= Nothing)
            readChunks [] >>= (@?= Just "")
        ]
  where
    declaring size = defaultRequest{pathInfo = ["deleted"], requestBodyLength = KnownLength (fromIntegral size)}
    statusFor request' = do
        status <- newIORef Nothing
        _ <- application request' $ \response -> do
            writeIORef status (Just (statusCode (responseStatus response)))
            pure ResponseReceived
        readIORef status
    readChunks chunks = do
        remaining <- newIORef chunks
        let next = atomicModifyIORef' remaining $ \case
                chunk : rest -> (rest, chunk)
                [] -> ([], "")
        readLimitedBody (setRequestBodyChunks next defaultRequest)

warpTests :: TestTree
warpTests =
    testGroup
        "Warp"
        [ testCase "an unsplittable request line is closed without a reply or a log" $ do
            (reply, messages) <- serveLogged application $ \port ->
                exchangeBytes port ("GET /b/" <> bearerShaped <> "\r\n\r\n")
            reply @?= ""
            messages @?= []
        , testCase "an oversized header block gets a 400 with the headers and no log" $ do
            (response, messages) <- serveLogged application $ \port ->
                exchange port (request "GET" ("/b/" <> bearerShaped) ["X-Filler: " <> ByteString.replicate (51 * 1024) 97] "")
            rawStatus response @?= 400
            assertPageHeaders response
            messages @?= []
        , testCase "an uncaught exception gets a 500 with the headers and a type-only log" $ do
            (response, messages) <- serveLogged failing $ \port ->
                exchange port (request "GET" ("/b/" <> bearerShaped) [] "")
            rawStatus response @?= 500
            assertPageHeaders response
            messages @?= ["A request failed with an exception of type ErrorCall."]
            assertNoBearer messages
        ]
  where
    failing :: Application
    failing _ _ = throwIO (ErrorCall ("failed for /b/" <> Char8.unpack bearerShaped))

-- | A path segment in the bearer format, which must never reach a log.
bearerShaped :: ByteString
bearerShaped = "v2.AAAAAAAAAAAAAAAAAAAAAA.secretsecretsecretsecretsecretsecretsecr"

withServer :: ((ByteString -> IO RawResponse) -> IO a) -> IO a
withServer action =
    testWithApplicationSettings (serverSettings loopback (const (pure ()))) (pure application) $ \port ->
        action (exchange (fromIntegral port))

-- | Serve without rethrowing application exceptions, so Warp's hooks handle them, and capture operator messages.
serveLogged :: Application -> (PortNumber -> IO a) -> IO (a, [Text])
serveLogged app action = do
    messages <- newIORef []
    let logMessage message = atomicModifyIORef' messages (\logged -> (logged <> [message], ()))
    result <- withApplicationSettings (serverSettings loopback logMessage) (pure app) (action . fromIntegral)
    logged <- readIORef messages
    pure (result, logged)

loopback :: Listener
loopback = Listener "127.0.0.1" 0

assertPageHeaders :: RawResponse -> Assertion
assertPageHeaders response = do
    mapM_ (\(name, value) -> (name, header name response) @?= (name, [value])) securityHeaders
    header "Content-Type" response @?= ["text/html; charset=utf-8"]
    header "Set-Cookie" response @?= []

assertNoBearer :: [Text] -> Assertion
assertNoBearer messages =
    assertBool "a message contains the bearer" . not $
        any (Text.isInfixOf "secretsecret") messages
