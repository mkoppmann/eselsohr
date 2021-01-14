module Lib.Web.Controller.Command
  ( command,
  )
where

import Lib.Core.Domain (Accesstoken)
import qualified Lib.Web.Route.Command as Route
import Lib.Web.Types (AppServer)
import Servant.API (NoContent)

command :: Route.CommandSite AppServer
command =
  Route.CommandSite
    { Route.deleteApi = deleteApi,
      Route.patchApi = patchApi,
      Route.postApi = postApi
    }

deleteApi :: (Monad m) => Maybe Accesstoken -> m NoContent
deleteApi _ = undefined

patchApi :: (Monad m) => Maybe Accesstoken -> m NoContent
patchApi _ = undefined

postApi :: (Monad m) => Maybe Accesstoken -> m NoContent
postApi _ = undefined
