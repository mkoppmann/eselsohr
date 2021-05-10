module Lib.Web.Route.Collection
  ( Collections
  , CollectionsSite(..)
  ) where

import           Lib.Web.Types                  ( Redirection
                                                , ToApi
                                                )
import           Servant                        ( (:>)
                                                , Post
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )
import           Servant.HTML.Lucid             ( HTML )

newtype CollectionsSite route = CollectionsSite
  { createCollection ::
      route
        :- "api"
        :> "collections"
        :> Post '[HTML] Redirection
  }
  deriving stock (Generic)

type Collections = ToApi CollectionsSite
