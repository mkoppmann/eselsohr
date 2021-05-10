module Lib.Web.Controller.Collection
  ( collection
  ) where

import           Lib.App                        ( WithError )
import           Lib.Core.Effect                ( MonadRandom
                                                , RWState
                                                )
import qualified Lib.Core.Service.Collection   as Service
import           Lib.Web.Controller.Util        ( redirect )
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( AppServer
                                                , Redirection
                                                )
import           Servant                        ( fieldLink )

collection :: Route.CollectionsSite AppServer
collection =
  Route.CollectionsSite { Route.createCollection = createCollection }

createCollection :: (RWState m, MonadRandom m, WithError m) => m Redirection
createCollection = do
  acc <- Service.createCollection
  redirect . Route.linkAsText . fieldLink Route.collectionOverview $ Just acc
