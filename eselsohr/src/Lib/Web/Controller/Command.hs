module Lib.Web.Controller.Command
  ( command
  ) where

import           Lib.App                        ( WithError )
import qualified Lib.Core.Action               as Action
import           Lib.Core.Domain                ( Accesstoken )
import           Lib.Core.Effect                ( MonadRandom
                                                , WriteState
                                                )
import qualified Lib.Web.Controller.Common     as CC
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( AppServer )
import           Servant                        ( NoContent(..) )

command :: Route.CommandSite AppServer
command = Route.CommandSite { Route.createResourceApi = createResourceApi
                            , Route.deleteApi         = deleteApi
                            , Route.patchApi          = patchApi
                            , Route.postApi           = postApi
                            }

createResourceApi :: (WriteState m, MonadRandom m) => m NoContent
createResourceApi = Action.createResource >> pure NoContent

deleteApi :: (WithError m) => Maybe Accesstoken -> m NoContent
deleteApi _ = CC.notImplemented

patchApi :: (WithError m) => Maybe Accesstoken -> m NoContent
patchApi _ = CC.notImplemented

postApi :: (WithError m) => Maybe Accesstoken -> m NoContent
postApi _ = CC.notImplemented
