module Lib.Web.Controller.SharedReference where

import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( Capability
                                                , Id
                                                , render
                                                )
import qualified Lib.Core.Domain.Capability    as Cap
import           Lib.Core.Effect                ( MonadTime
                                                , RWState
                                                )
import qualified Lib.Core.Service              as Service
import           Lib.Web.Controller.Util        ( authAction
                                                , getContextState
                                                , getObjRef
                                                , redirect
                                                )
import qualified Lib.Web.Route.SharedReference as Route
import           Lib.Web.Types                  ( AppServer
                                                , DeleteItemForm(..)
                                                , Redirection
                                                )

sharedRef :: Route.SharedRefsSite AppServer
sharedRef = Route.SharedRefsSite { Route.deleteSharedRef = deleteSharedRef }

deleteSharedRef
  :: (RWState m, MonadTime m, WithError m)
  => Id Capability
  -> DeleteItemForm
  -> m Redirection
deleteSharedRef sharedRefId DeleteItemForm {..} = do
  ctxState <- getContextState acc
  let mAuthAct = Cap.deleteSharedRef (getObjRef ctxState) sharedRefId
  authAction (Service.deleteSharedRef ctxState) mAuthAct
  redirect $ render goto
