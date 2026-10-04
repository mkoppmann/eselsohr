{- |
Module      : RawHttp
Description : Raw HTTP exchanges with a local test listener

'exchange' connects to a port on 127.0.0.1, sends the given bytes unchanged, and reads the reply until its @Content-Length@ is complete, the server closes the connection, or five seconds pass.
Because the bytes are sent unchanged, tests can send malformed request lines that no HTTP client library would produce.
Warp keeps the connection open after its own error responses even when the request asked to close it, so the reply ends at its declared length rather than at the close.
Well-formed requests carry @Connection: close@ so that replies without a length end too.

The reply is split into its status code, its headers with case-insensitive names, and the bytes after the header block.
'exchangeBytes' returns the raw reply instead, which is empty when the server closes without answering.
-}
module RawHttp
    ( RawResponse (..)
    , exchange
    , exchangeBytes
    , request
    , header
    ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

data RawResponse = RawResponse
    { rawStatus :: Int
    , rawHeaders :: [(CI ByteString, ByteString)]
    , rawBody :: ByteString
    }
    deriving stock (Show)

-- | A well-formed request with a method, path, extra header lines, and body.
request :: ByteString -> ByteString -> [ByteString] -> ByteString -> ByteString
request method path headers body =
    ByteString.concat
        [ method <> " " <> path <> " HTTP/1.1\r\n"
        , "Host: localhost\r\n"
        , "Connection: close\r\n"
        , ByteString.concat (map (<> "\r\n") headers)
        , "\r\n"
        , body
        ]

exchange :: PortNumber -> ByteString -> IO RawResponse
exchange port bytes = exchangeBytes port bytes >>= either fail pure . parseResponse

exchangeBytes :: PortNumber -> ByteString -> IO ByteString
exchangeBytes port bytes = do
    reply <- timeout 5000000 . bracket open close $ \socket' -> do
        sendAll socket' bytes
        receive socket' ""
    maybe (fail "no complete reply within five seconds") pure reply
  where
    open = do
        socket' <- socket AF_INET Stream defaultProtocol
        connect socket' (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
        pure socket'
    receive socket' received
        | complete received = pure received
        | otherwise = do
            chunk <- recv socket' 65536
            if ByteString.null chunk then pure received else receive socket' (received <> chunk)

-- | Whether a reply holds its header block and as many body bytes as its @Content-Length@ declares.
complete :: ByteString -> Bool
complete received = case parseResponse received of
    Right response
        | "\r\n\r\n" `ByteString.isInfixOf` received
        , [value] <- header "Content-Length" response
        , Just (size, "") <- Char8.readInt value ->
            ByteString.length (rawBody response) >= size
    _ -> False

parseResponse :: ByteString -> Either String RawResponse
parseResponse reply = do
    let (head', rest) = ByteString.breakSubstring "\r\n\r\n" reply
    (statusLine, headerLines) <- case Char8.lines (Char8.filter (/= '\r') head') of
        line : others -> Right (line, others)
        [] -> Left "empty reply"
    status <- case Char8.words statusLine of
        _ : code : _ | Just (value, "") <- Char8.readInt code -> Right value
        _ -> Left ("malformed status line: " <> show statusLine)
    pure (RawResponse status (map field headerLines) (ByteString.drop 4 rest))
  where
    field line =
        let (name, value) = Char8.break (== ':') line
         in (CI.mk name, Char8.dropWhile (== ' ') (ByteString.drop 1 value))

-- | Every value of a header in a response.
header :: CI ByteString -> RawResponse -> [ByteString]
header name = map snd . filter ((== name) . fst) . rawHeaders
