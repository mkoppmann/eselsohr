module Lib.Web.Controller.Query
  ( query,
  )
where

import Data.Aeson (Value)
import Lib.Core.Domain (Accesstoken)
import qualified Lib.Web.Route as Route
import Lib.Web.Types (AppServer)

query :: Route.QuerySite AppServer
query =
  Route.QuerySite
    { Route.query = query'
    }

query' :: (Monad m) => Maybe Accesstoken -> m Value
query' _ = undefined
