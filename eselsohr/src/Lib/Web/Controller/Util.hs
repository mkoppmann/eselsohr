module Lib.Web.Controller.Util
  ( getContextState
  , getObjRef
  , authAction
  , notImplemented
  , invalid
  , redirect
  , serverError
  , notAuthorized
  , missingParameter
  ) where

import           Lib.App                        ( WithError
                                                , redirect303
                                                , throwError
                                                )
import qualified Lib.App.Error                 as Error
import           Lib.Core.Domain                ( Accesstoken
                                                , AuthAction
                                                , Capability(..)
                                                , Context(..)
                                                , ExpirationDate(..)
                                                , Id
                                                , ObjectReference
                                                , Reference(..)
                                                , Resource
                                                , toReference
                                                )

import qualified Lib.Core.Domain.Entity        as Entity
import           Lib.Core.Effect                ( ContextState(..)
                                                , MonadTime(..)
                                                , ReadState(..)
                                                , SealedResource
                                                )
import qualified Lib.Core.Effect.Repository    as R
import           Lib.Web.Types                  ( Redirection )

getContextState
  :: (ReadState m, MonadTime m, WithError m) => Accesstoken -> m ContextState
getContextState acc = do
  let ref   = toReference acc
      resId = resourceId ref
  sRes   <- R.load resId

  capEnt <- case R.noAuthLookupCap sRes $ capabilityId ref of
    Nothing     -> serverError "Could not find capability"
    Just capEnt -> pure capEnt
  let objRef = objectRef $ Entity.val capEnt

  case capExpirationDate $ Entity.val capEnt of
    Nothing      -> pure $ mkContextState resId sRes objRef
    Just expDate -> getCurrentTime >>= \currTime ->
      if unExpirationDate expDate < currTime
        then serverError "Capability has expired"
        else pure $ mkContextState resId sRes objRef
 where
  mkContextState
    :: Id Resource -> SealedResource -> ObjectReference -> ContextState
  mkContextState resId sRes capEnt = ContextState (Context resId capEnt) sRes

getObjRef :: ContextState -> ObjectReference
getObjRef = ctxObjRef . csContext

authAction :: (WithError m) => (AuthAction -> m ()) -> Maybe AuthAction -> m ()
authAction = maybe notAuthorized

notImplemented :: (WithError m) => m a
notImplemented = throwError $ Error.serverError "Not implemented yet"

invalid :: (WithError m) => Text -> m a
invalid pName =
  throwError . Error.invalid $ "The " <> pName <> "parameter is invalid."

redirect :: (WithError m) => Text -> m Redirection
redirect = throwError . redirect303

serverError :: (WithError m) => Text -> m a
serverError = throwError . Error.serverError

notAuthorized :: (WithError m) => m a
notAuthorized = throwError $ Error.notAuthorized
  "You are missing the required permissions for this action"

missingParameter :: (WithError m) => Text -> m a
missingParameter pName =
  throwError
    .  Error.missingParameter
    $  "The following parameter is missing: "
    <> pName
