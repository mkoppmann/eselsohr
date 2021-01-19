{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Module : Network.Wai.Middleware.MyMethodOverride
--
-- Changes the request-method via parameters `_commandMethod` and `_queryMethod`.
-- Code taken from `MethodOverridePost`, but modified to convert HTTP body
-- parameters to query arguments when `_queryMethod` is GET.
-- TODO: Likewise, when the first query argument in a GET request is `_commandMethod`,
-- the arguments are converted to HTTP body parameters.
--
-- * Source: https://github.com/yesodweb/wai/blob/06aa52b3255f38d22e6b78c8d68cf51fe4552cce/wai-extra/Network/Wai/Middleware/MethodOverridePost.hs
--
-- * License: https://github.com/yesodweb/wai/blob/master/LICENSE
module Network.Wai.Middleware.MyMethodOverride
  ( methodOverride,
  )
where

import Data.ByteString.Lazy (toChunks)
import Data.List (lookup)
import Network.HTTP.Types (Query, hContentType, parseQuery)
import Network.Wai (Middleware, Request (..), lazyRequestBody)

-- | Allows overriding of the HTTP request method via the `_commandMethod` and
-- `_queryMethod` post string parameter.
--
-- * Looks for the Content-Type requestHeader.
--
-- * If the header is set to application/x-www-form-urlencoded
-- and the first POST parameter is `_commandMethod` then it changes the
-- request-method to the value of that parameter.
--
-- * If the second parameter is set to `_queryMethod` and the path does not
-- start with `api`, the request-method is rewritten to that.
--
-- * If the value set in `_queryMethod` is `GET`, parameters in the body are
-- also set as query parameters.
methodOverride :: Middleware
methodOverride app req send =
  case (requestMethod req, lookup hContentType (requestHeaders req)) of
    ("POST", Just "application/x-www-form-urlencoded") -> setPost req >>= flip app send
    _otherRequestMethod -> app req send

setPost :: Request -> IO Request
setPost req = do
  body <- mconcat . toChunks <$> lazyRequestBody req
  ref <- newIORef body
  let rb = atomicModifyIORef ref $ (,) mempty
  case parseQuery body of
    (("_commandMethod", Just commandMethod) : ("_queryMethod", Just queryMethod) : remainingQueries) -> do
      case pathInfo req of
        "api" : _ -> rewritePost req rb commandMethod remainingQueries
        "collection" : _ -> rewritePost req rb queryMethod $ queryString req
        _otherPaths -> rewritePost req rb queryMethod remainingQueries
    _differentParameters ->
      return $ req {requestBody = rb}

rewritePost :: Request -> IO ByteString -> ByteString -> Query -> IO Request
rewritePost req rb newmethod remainingQueries = do
  case newmethod of
    "GET" ->
      return req {requestBody = rb, requestMethod = newmethod, queryString = remainingQueries}
    _notGET ->
      return req {requestBody = rb, requestMethod = newmethod}
