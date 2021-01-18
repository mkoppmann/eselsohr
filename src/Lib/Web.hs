module Lib.Web
  ( Api,
    application,
  )
where

import Lib.App (AppEnv, runAppAsHandler)
import qualified Lib.Web.Controller as Controller (command, frontend, query)
import Lib.Web.Route (Command, Frontend, Query)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.MyMethodOverride (methodOverride)
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

application :: AppEnv -> Application
application env =
  gzip def
    . addHeaders securityHeaders
    . methodOverride
    $ serve (Proxy @Api) (server env)

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
