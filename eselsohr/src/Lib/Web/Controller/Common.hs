module Lib.Web.Controller.Common
  ( getContextState,
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
import Lib.Core.Effect.Repository (ContextState (..), ReadState (..), SealedResource)
import qualified Lib.Core.Effect.Repository as R
import Lib.Core.Effect.Time (MonadTime (..))

getContextState ::
  (ReadState m, MonadTime m) => Accesstoken -> m (Either Text ContextState)
getContextState acc = do
  let ref = toReference acc
  res <- R.load $ resourceId ref

  case R.lookupCap res $ capabilityId ref of
    Nothing -> pure $ Left "Could not find capability"
    Just capEnt -> do
      case capExpirationDate $ Entity.val capEnt of
        Nothing -> pure $ fetchAction ref res capEnt
        Just expDate -> do
          currTime <- getCurrentTime
          if unExpirationDate expDate < currTime
            then pure $ Left "Capability has expired"
            else pure $ fetchAction ref res capEnt
  where
    fetchAction ::
      Reference -> SealedResource -> Entity Capability -> Either Text ContextState
    fetchAction ref res capEnt =
      let actId = actionId $ Entity.val capEnt
       in case R.lookupAct res actId of
            Nothing -> Left "Could not find action"
            Just act -> Right $ ContextState (Context ref capEnt act) res

getCommand :: (WithError m) => ContextState -> m ContextState
getCommand ctx =
  let context = csContext ctx
   in case Entity.val $ ctxAct context of
        Frontend fAction -> do
          actEnt <- R.getOneAct (csResource ctx) $ command fAction
          pure $ ctx {csContext = context {ctxAct = actEnt}}
        Command _cAction -> pure ctx
        Query _qAction ->
          throwError $ serverError "Expected Command but Query was given"

getQuery :: (WithError m) => ContextState -> m ContextState
getQuery ctx =
  let context = csContext ctx
   in case Entity.val $ ctxAct context of
        Frontend fAction -> do
          actEnt <- R.getOneAct (csResource ctx) $ query fAction
          pure $ ctx {csContext = context {ctxAct = actEnt}}
        Query _qAction -> pure ctx
        Command _cAction ->
          throwError $ serverError "Expected Query but Command was given"

getAction :: (WithError m) => ContextState -> Id Action -> m Action
getAction ctx actId = Entity.val <$> R.getOneAct (csResource ctx) actId

getRedirectTo :: Action -> Maybe Text
getRedirectTo = \case
  Frontend fAction -> Just $ redirectTo fAction
  _nonFrontendAction -> Nothing

notImplemented :: (WithError m) => m a
notImplemented = throwError $ serverError "Not implemented yet"
