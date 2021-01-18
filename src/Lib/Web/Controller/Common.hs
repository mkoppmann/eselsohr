module Lib.Web.Controller.Common
  ( getContext,
    getCommand,
    getQuery,
    getRedirectTo,
    getAction,
    notImplemented,
  )
where

import Lib.App.Error (WithError, serverError, throwError)
import Lib.Core.Domain.Accesstoken (Accesstoken, Reference (..), toReference)
import Lib.Core.Domain.Capability (Action (..), Capability (..), FrontendAction (..))
import Lib.Core.Domain.Context (Context (..))
import Lib.Core.Domain.Entity (Entity)
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Domain.ExpirationDate (ExpirationDate (..))
import Lib.Core.Domain.Id (Id)
import Lib.Core.Domain.Resource (Resource)
import Lib.Core.Effect (MonadTime (..), ReadCapabilities (..))
import qualified Lib.Core.Effect.Repository as R

getContext ::
  (ReadCapabilities m, MonadTime m) =>
  Accesstoken ->
  m (Either Text Context)
getContext acc = do
  let ref = toReference acc
  let resId = resourceId ref
  let capId = capabilityId ref
  mCap <- R.lookupCap resId capId
  case mCap of
    Nothing -> pure $ Left "Could not find capability"
    Just capEnt -> do
      case capExpirationDate $ Entity.val capEnt of
        Nothing -> fetchAction ref resId capEnt
        Just expDate -> do
          currTime <- getCurrentTime
          if unExpirationDate expDate < currTime
            then pure $ Left "Capabillity has expired"
            else fetchAction ref resId capEnt
  where
    fetchAction ::
      (ReadCapabilities m) =>
      Reference ->
      Id Resource ->
      Entity Capability ->
      m (Either Text Context)
    fetchAction ref resId capEnt = do
      let actId = actionId $ Entity.val capEnt
      mAct <- R.lookupAct resId actId
      case mAct of
        Nothing -> pure $ Left "Could not find action"
        Just act -> pure . Right $ Context ref capEnt act

getCommand :: (ReadCapabilities m) => Context -> m Context
getCommand ctx = do
  let resId = resourceId $ ctxRef ctx
  let act = Entity.val $ ctxAct ctx
  case act of
    Frontend fAction -> do
      actEnt <- R.getOneAct resId $ command fAction
      let newCtx = ctx {ctxAct = actEnt}
      pure newCtx
    _nonFrontendAction -> pure ctx

getQuery :: (ReadCapabilities m) => Context -> m Context
getQuery ctx = do
  let resId = resourceId $ ctxRef ctx
  let act = Entity.val $ ctxAct ctx
  case act of
    Frontend fAction -> do
      actEnt <- R.getOneAct resId $ query fAction
      let newCtx = ctx {ctxAct = actEnt}
      pure newCtx
    _nonFrontendAction -> pure ctx

getAction :: (ReadCapabilities m) => Context -> Id Action -> m Action
getAction ctx actId = do
  let resId = resourceId $ ctxRef ctx
  Entity.val <$> R.getOneAct resId actId

getRedirectTo :: Action -> Maybe Text
getRedirectTo = \case
  Frontend fAction -> Just $ redirectTo fAction
  _nonFrontendAction -> Nothing

notImplemented :: (WithError m) => m a
notImplemented = throwError $ serverError "Not implemented yet"
