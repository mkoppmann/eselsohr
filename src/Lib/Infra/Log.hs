-- | Logging action for the project. Currently just logs the output to terminal.
module Lib.Infra.Log
    ( mainLogAction
    , runAppAsHandler
    , runAppLogIO
    , runAppLogIO_
    , WithLog
    , module Colog
    ) where

import qualified Colog
    ( Message
    , Msg (..)
    , WithLog
    , filterBySeverity
    , richMessageAction
    )

import Colog
    ( LogAction (..)
    , Severity (..)
    , log
    , pattern D
    , pattern E
    , pattern I
    , pattern W
    )
import Control.Monad.Except (liftEither)
import Servant (Handler)

import Lib.Domain.Error (isRedirect)
import Lib.Infra.Error
    ( AppError (appErrorType)
    , toHttpError
    )
import Lib.Infra.Monad
    ( App
    , AppEnv
    , runAppAsIO
    )

-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Colog.Message m

{- | Main log action for the application. Prints message with some metadata to
 @stdout@.
-}
mainLogAction :: MonadIO m => Severity -> LogAction m Colog.Message
mainLogAction severity = Colog.filterBySeverity severity Colog.msgSeverity Colog.richMessageAction

----------------------------------------------------------------------------
-- Application runners with runners
----------------------------------------------------------------------------

-- | Runs application as servant 'Handler'.
runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = liftEither . first toHttpError =<< liftIO (runAppLogIO env app)

-- | Runs application like 'runAppAsIO' but also logs error.
runAppLogIO :: AppEnv -> App a -> IO (Either AppError a)
runAppLogIO env app = do
    appRes <- runAppAsIO env app
    logRes <- whenLeft (Right ()) appRes $ logMPErrorIO env
    pure $ appRes <* logRes

-- | Like 'runAppAsIO' but discards result.
runAppLogIO_ :: AppEnv -> App a -> IO ()
runAppLogIO_ env app = void $ runAppLogIO env app

----------------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------------

{- Logs the given 'AppError' before running the route handlers in IO. Redirects
 are no real errors so they don’t get logged.
-}
logMPErrorIO :: AppEnv -> AppError -> IO (Either AppError ())
logMPErrorIO env err = if isRedirect err.appErrorType then runAppAsIO env pass else runAppAsIO env . log E $ show err
