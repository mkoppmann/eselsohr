module Lib.Core.Flow.Query
  ( queryAction
  ) where

import           Data.Aeson                     ( Value )
import           Lib.App                        ( WithError
                                                , serverError
                                                , throwError
                                                )
import           Lib.Core.Effect                ( ContextState )

queryAction :: (WithError m) => ContextState -> m Value
queryAction _ = throwError $ serverError "Not implemented yet"
