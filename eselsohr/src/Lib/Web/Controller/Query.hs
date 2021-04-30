module Lib.Web.Controller.Query
  ( query
  ) where

import           Data.Aeson                     ( Value )
import           Lib.App.Error                  ( WithError )
import           Lib.Core.Domain.Accesstoken    ( Accesstoken )
import qualified Lib.Web.Controller.Common     as CC
import qualified Lib.Web.Route.Query           as Route
import           Lib.Web.Types                  ( AppServer )

query :: Route.QuerySite AppServer
query = Route.QuerySite { Route.query = query' }

query' :: (WithError m) => Maybe Accesstoken -> m Value
query' _ = CC.notImplemented
