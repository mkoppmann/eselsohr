module Lib.Web.Route.Query
  ( Query,
    QuerySite (..),
  )
where

import Data.Aeson (Value)
import Lib.Core.Domain (Accesstoken)
import Lib.Web.Types (ToApi)
import Servant (Get, Header, JSON, (:>))
import Servant.API.Generic (GenericMode ((:-)))

newtype QuerySite route = QuerySite
  { query ::
      route
        :- "api"
        :> "cap"
        :> Header "Authorization" Accesstoken
        :> Get '[JSON] Value
  }
  deriving stock (Generic)

type Query = ToApi QuerySite
