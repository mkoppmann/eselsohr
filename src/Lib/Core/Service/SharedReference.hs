module Lib.Core.Service.SharedReference
  ( getSharedRefs
  , createSharedRef
  , deleteSharedRef
  ) where

import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( AuthAction
                                                , Capability(..)
                                                , ExpirationDate
                                                , Id
                                                , ObjectReference
                                                , Reference(..)
                                                , Resource
                                                , Revocable
                                                , mkAccesstoken
                                                )
import           Lib.Core.Effect                ( ContextState(..)
                                                , WriteState(..)
                                                )
import qualified Lib.Core.Effect.Repository    as R
import           Lib.Core.Service.Util          ( createCapability
                                                , getResId
                                                )
import           Relude.Extra                   ( secondF )

getSharedRefs
  :: (WithError m)
  => ContextState
  -> (ObjectReference -> Bool)
  -> AuthAction
  -> m (Seq (Capability, Revocable))
getSharedRefs ctxState sharedRefFilter authAction =
  secondF (toRevocable $ getResId ctxState)
    .   fmap swap
    .   Seq.fromList
    .   Map.toList
    .   Map.filter (sharedRefFilter . objectRef)
    <$> R.getAllCap (csResource ctxState) authAction
 where
  toRevocable :: Id Resource -> Id Capability -> Revocable
  toRevocable resId capId = (capId, mkAccesstoken $ Reference resId capId)

createSharedRef
  :: (WriteState m, WithError m)
  => ContextState
  -> Maybe Text
  -> Maybe ExpirationDate
  -> ObjectReference
  -> AuthAction
  -> m ()
createSharedRef ctxState petname mExpDate sharedRef =
  createCapability (getResId ctxState) sharedRef petname mExpDate

deleteSharedRef
  :: (WriteState m, WithError m) => ContextState -> AuthAction -> m ()
deleteSharedRef ctxState authAct =
  R.commit (getResId ctxState) . one =<< R.deleteCap authAct
