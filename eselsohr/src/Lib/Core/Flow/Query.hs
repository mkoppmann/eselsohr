module Lib.Core.Flow.Query
  ( queryAction,
  )
where

import Data.Aeson (Value)
import Lib.App.Error (WithError, serverError, throwError)
import Lib.Core.Effect.Repository (ContextState)

queryAction :: (WithError m) => ContextState -> m Value
queryAction _ = throwError $ serverError "Not implemented yet"
