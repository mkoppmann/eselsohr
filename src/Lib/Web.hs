module Lib.Web
  ( Api,
    application,
  )
where

import Lib.App (AppEnv, runAppAsHandler)
import qualified Lib.Web.Controller as Controller (command, frontend, query)
import qualified Lib.Web.Route as Route (Command, Frontend, Query)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.MyMethodOverride (methodOverride)
import Servant.API ((:<|>) (..))
import Servant.API.Generic (toServant)
import Servant.Server (Application, Server, hoistServer, serve)

type Api = Route.Frontend :<|> Route.Query :<|> Route.Command

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
  gzip def $ methodOverride $ serve (Proxy @Api) (server env)
