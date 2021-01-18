module Lib.Web.Controller.Command
  ( command,
  )
where

import Lib.App.Error (WithError)
import Lib.Core.Domain.Accesstoken (Accesstoken)
import qualified Lib.Web.Controller.Common as CC
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

deleteApi :: (WithError m) => Maybe Accesstoken -> m NoContent
deleteApi _ = CC.notImplemented

patchApi :: (WithError m) => Maybe Accesstoken -> m NoContent
patchApi _ = CC.notImplemented

postApi :: (WithError m) => Maybe Accesstoken -> m NoContent
postApi _ = CC.notImplemented
