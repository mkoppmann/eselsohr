module Lib.Web
  ( Api
  , application
  ) where

import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , Hsts(..)
                                                , Https(..)
                                                , runAppAsHandler
                                                )
import qualified Lib.Web.Controller            as Controller

import           Lib.Web.Route                  ( Articles
                                                , Collections
                                                , Frontend
                                                , SharedRefs
                                                , UnlockLinks
                                                )
import           Network.Wai                    ( Middleware )
import           Network.Wai.Handler.Warp       ( Port )
import           Network.Wai.Middleware.AddHeaders
                                                ( addHeaders )
import           Network.Wai.Middleware.AddHsts ( addHsts )
import qualified Network.Wai.Middleware.EnforceHTTPS
                                               as EnforceHTTPS
import           Network.Wai.Middleware.Gzip    ( def
                                                , gzip
                                                )
import           Network.Wai.Middleware.MethodOverridePost
                                                ( methodOverridePost )
import           Network.Wai.Middleware.NoOp    ( noOp )
import           Network.Wai.Middleware.RealIp  ( realIpHeader )

import           Servant                        ( (:<|>)(..)
                                                , Application
                                                , Server
                                                , hoistServer
                                                , serve
                                                )
import           Servant.API.Generic            ( toServant )

type Api
  = Frontend :<|> Articles :<|> Collections :<|> UnlockLinks :<|> SharedRefs

server :: AppEnv -> Server Api
server env =
  hoistServer (Proxy @Api) (runAppAsHandler env)
    $    toServant Controller.frontend
    :<|> toServant Controller.article
    :<|> toServant Controller.collection
    :<|> toServant Controller.unlockLink
    :<|> toServant Controller.sharedRef

application :: Port -> AppEnv -> Application
application port env@Env {..} =
  -- Response middlewares
  gzip def
    . hstsHeader
    . realIpHeader "X-Forwarded-For"
    . addHeaders securityHeaders
    -- Request middlewares
    . enforceHttps
    . methodOverridePost
    $ serve (Proxy @Api) (server env)
 where
  enforceHttps :: Middleware
  enforceHttps = case envHttps of
    HttpsOn -> EnforceHTTPS.withConfig
      $ EnforceHTTPS.defaultConfig { EnforceHTTPS.httpsPort = port }
    HttpsOff -> noOp

  hstsHeader :: Middleware
  hstsHeader = case envHsts of
    HstsOn  -> addHsts
    HstsOff -> noOp

securityHeaders :: [(ByteString, ByteString)]
securityHeaders =
  [ ("Referrer-Policy", "no-referrer")
  , ("X-Content-Type-Options", "nosniff")
  , ( "Content-Security-Policy"
    , "default-src 'none';\
      \ style-src 'self';\
      \ img-src 'self';\
      \ form-action 'self';\
      \ upgrade-insecure-requests;"
    )
  ]
