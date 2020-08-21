module Lib.Web
  ( Api,
    application,
  )
where

import Lib.App (AppEnv)
import Lib.Effect.Log (runAppAsHandler)
import Lib.Web.Handler (actionServer, frontendServer)
import Lib.Web.Route (ActionApi, FrontendApi)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.MyMethodOverridePost (methodOverridePost)
import Servant.API ((:<|>) (..))
import Servant.API.Generic (toServant)
import Servant.Server (Application, Server, hoistServer, serve)

type Api = FrontendApi :<|> ActionApi

server :: AppEnv -> Server Api
server env =
  hoistServer
    (Proxy @Api)
    (runAppAsHandler env)
    $ toServant frontendServer :<|> toServant actionServer

application :: AppEnv -> Application
application env =
  gzip def $ methodOverridePost $ serve (Proxy @Api) (server env)
