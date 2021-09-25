module Lib.Web.Route.Collection
  ( Collections
  , CollectionsSite(..)
  ) where

import           Lib.Web.Types                  ( PostCreateSharedOverviewRefForm
                                                , Redirection
                                                , ToApi
                                                )
import           Servant                        ( (:>)
                                                , FormUrlEncoded
                                                , Post
                                                , ReqBody
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )
import           Servant.HTML.Lucid             ( HTML )

data CollectionsSite route = CollectionsSite
  { createCollection ::
      route
        :- "api"
        :> "collections"
        :> Post '[HTML] Redirection
  , createSharedOverviewRef ::
      route
        :- "api"
        :> "collections"
        :> "shared-ref"
        :> ReqBody '[FormUrlEncoded] PostCreateSharedOverviewRefForm
        :> Post '[HTML] Redirection
  }
  deriving stock (Generic)

type Collections = ToApi CollectionsSite
