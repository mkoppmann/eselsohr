module Lib.Web.Controller.Common
  ( getContextState
  , getAction
  , notImplemented
  ) where

import           Lib.App                        ( WithError
                                                , serverError
                                                , throwError
                                                )
import           Lib.Core.Domain                ( Accesstoken
                                                , Action(..)
                                                , Capability(..)
                                                , Context(..)
                                                , Entity
                                                , ExpirationDate(..)
                                                , Id
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

getContextState
  :: (ReadState m, MonadTime m) => Accesstoken -> m (Either Text ContextState)
getContextState acc = do
  let ref   = toReference acc
      resId = resourceId ref
  res <- R.load resId

  case R.lookup res $ capabilityId ref of
    Nothing     -> pure $ Left "Could not find capability"
    Just capEnt -> case capExpirationDate $ Entity.val capEnt of
      Nothing      -> pure $ fetchAction resId res capEnt
      Just expDate -> getCurrentTime >>= \currTime ->
        if unExpirationDate expDate < currTime
          then pure $ Left "Capability has expired"
          else pure $ fetchAction resId res capEnt
 where
  fetchAction
    :: Id Resource
    -> SealedResource
    -> Entity Capability
    -> Either Text ContextState
  fetchAction resId res capEnt =
    case R.lookup res . actionId $ Entity.val capEnt of
      Nothing  -> Left "Could not find action"
      Just act -> Right $ ContextState (Context resId capEnt act) res

getAction :: (WithError m) => ContextState -> Id Action -> m Action
getAction ctx actId = Entity.val <$> R.getOne (csResource ctx) actId

notImplemented :: (WithError m) => m a
notImplemented = throwError $ serverError "Not implemented yet"
