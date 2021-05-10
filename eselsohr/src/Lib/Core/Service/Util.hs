module Lib.Core.Service.Util
  ( getResId
  , notAuthorized
  , notFound
  ) where

import           Lib.App                        ( WithError
                                                , throwError
                                                )
import qualified Lib.App.Error                 as Error
import           Lib.Core.Domain                ( Context(..)
                                                , Id
                                                , Resource
                                                )
import           Lib.Core.Effect                ( ContextState(..) )

getResId :: ContextState -> Id Resource
getResId = ctxResId . csContext

notAuthorized :: (WithError m) => m a
notAuthorized = throwError $ Error.notAuthorized
  "You are missing the required permissions for this action"

notFound :: (WithError m) => m a
notFound = throwError Error.notFound
