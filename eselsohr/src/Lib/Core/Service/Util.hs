module Lib.Core.Service.Util
  ( getResId
  , createCapability
  , notAuthorized
  , notFound
  ) where

import qualified Data.Text                     as Text
import           Lib.App                        ( WithError
                                                , throwError
                                                )
import qualified Lib.App.Error                 as Error
import           Lib.Core.Domain                ( AuthAction
                                                , Capability(..)
                                                , Context(..)
                                                , ExpirationDate
                                                , Id
                                                , ObjectReference
                                                , Resource
                                                )
import           Lib.Core.Effect                ( ContextState(..)
                                                , WriteState
                                                )
import qualified Lib.Core.Effect.Repository    as R

getResId :: ContextState -> Id Resource
getResId = ctxResId . csContext

createCapability
  :: (WriteState m, WithError m)
  => Id Resource
  -> ObjectReference
  -> Maybe Text
  -> Maybe ExpirationDate
  -> AuthAction
  -> m ()
createCapability resId objRef petname mExpDate authAct = do
  let checkedPetname = checkForEmptyPetname petname
      cap            = Capability objRef checkedPetname mExpDate
  R.commit resId . one =<< R.insertCap authAct cap

checkForEmptyPetname :: Maybe Text -> Maybe Text
checkForEmptyPetname Nothing = Nothing
checkForEmptyPetname (Just pName) | Text.null pName = Nothing
                                  | otherwise       = Just pName

notAuthorized :: (WithError m) => m a
notAuthorized = throwError $ Error.notAuthorized
  "You are missing the required permissions for this action"

notFound :: (WithError m) => m a
notFound = throwError Error.notFound
