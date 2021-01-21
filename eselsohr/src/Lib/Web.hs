module Lib.Web
  ( Api,
    application,
  )
where

import Lib.App (AppEnv, Env (..), Hsts (..), Https (..), runAppAsHandler)
import qualified Lib.Web.Controller as Controller (command, frontend, query)
import Lib.Web.Route (Command, Frontend, Query)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.AddHsts (addHsts)
import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.MyMethodOverride (methodOverride)
import Network.Wai.Middleware.NoOp (noOp)
import Network.Wai.Middleware.RealIp (realIpHeader)
import Servant.API ((:<|>) (..))
import Servant.API.Generic (toServant)
import Servant.Server (Application, Server, hoistServer, serve)

type Api = Frontend :<|> Query :<|> Command

server :: AppEnv -> Server Api
server env =
  hoistServer
    (Proxy @Api)
    (runAppAsHandler env)
    $ toServant Controller.frontend
      :<|> toServant Controller.query
      :<|> toServant Controller.command

application :: Port -> AppEnv -> Application
application port env@Env {..} =
  -- Response middlewares
  gzip def
    . hstsHeader
    . realIpHeader "X-Forwarded-Proto"
    . addHeaders securityHeaders
    -- Request middlewares
    . enforceHttps
    . methodOverride
    $ serve (Proxy @Api) (server env)
  where
    enforceHttps = case envHttps of
      HttpsOn ->
        let config = EnforceHTTPS.defaultConfig {EnforceHTTPS.httpsPort = port}
         in EnforceHTTPS.withConfig config
      HttpsOff -> noOp

    hstsHeader = case envHsts of
      HstsOn -> addHsts
      HstsOff -> noOp

securityHeaders :: [(ByteString, ByteString)]
securityHeaders =
  [ ("Referrer-Policy", "no-referrer"),
    ("X-Content-Type-Options", "nosniff"),
    ( "Content-Security-Policy",
      "default-src 'none';\
      \ style-src 'self';\
      \ img-src 'self';\
      \ frame-ancestors https:\
      \ form-action 'self';\
      \ upgrade-insecure-requests;"
    )
  ]