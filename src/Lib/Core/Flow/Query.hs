module Lib.Core.Flow.Query
  ( queryAction,
  )
where

import Data.Aeson (Value)
import Lib.App.Error (WithError, serverError, throwError)
import Lib.Core.Domain.Context (Context)

queryAction :: (WithError m) => Context -> m Value
queryAction _ = throwError $ serverError "Not implemented yet"
