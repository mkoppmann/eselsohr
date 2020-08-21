{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Module : Network.Wai.Middleware.MyMethodOverridePost
--
-- Changes the request-method via first post-parameter _method.
-- Code taken from `MethodOverridePost`, but modified to convert HTTP body parameters
-- to query arguments when the method is GET.
--
-- * Source: https://github.com/yesodweb/wai/blob/06aa52b3255f38d22e6b78c8d68cf51fe4552cce/wai-extra/Network/Wai/Middleware/MethodOverridePost.hs
--
-- * License: https://github.com/yesodweb/wai/blob/master/LICENSE
module Network.Wai.Middleware.MyMethodOverridePost
  ( methodOverridePost,
  )
where

import Data.ByteString.Lazy (toChunks)
import Data.List (lookup)
import Network.HTTP.Types (hContentType, parseQuery)
import Network.Wai (Middleware, Request (..), lazyRequestBody)

-- | Allows overriding of the HTTP request method via the _method post string parameter.
--
-- * Looks for the Content-Type requestHeader.
--
-- * If the header is set to application/x-www-form-urlencoded
-- and the first POST parameter is _method
-- then it changes the request-method to the value of that
-- parameter.
--
-- * If the value set in `_method` is `GET`, parameters in the body are also set
-- as query parameters.
--
-- * This middleware only applies when the initial request method is POST.
methodOverridePost :: Middleware
methodOverridePost app req send =
  case (requestMethod req, lookup hContentType (requestHeaders req)) of
    ("POST", Just "application/x-www-form-urlencoded") -> setPost req >>= flip app send
    _otherRequestMethod -> app req send

setPost :: Request -> IO Request
setPost req = do
  body <- mconcat . toChunks <$> lazyRequestBody req
  ref <- newIORef body
  let rb = atomicModifyIORef ref $ (,) mempty
  case parseQuery body of
    (("_method", Just newmethod) : remainingQueries) -> do
      case newmethod of
        "GET" ->
          return req {requestBody = rb, requestMethod = newmethod, queryString = remainingQueries}
        _notGET ->
          return req {requestBody = rb, requestMethod = newmethod}
    _differentFirstParameter -> return $ req {requestBody = rb}
