-- | Logging action for the project. Currently just logs the output to terminal.
module Lib.App.Log
  ( mainLogAction,
    runAppAsHandler,
    runAppLogIO,
    runAppLogIO_,
    WithLog,
    module Colog,
  )
where

import Colog (LogAction (..), Severity (..), log, pattern D, pattern E, pattern I, pattern W)
import qualified Colog (Message, Msg (..), WithLog, filterBySeverity, richMessageAction)
import Control.Monad.Except (liftEither)
import Lib.App.Error (AppError (appErrorType), isRedirect, toHttpError)
import Lib.App.Monad (App, AppEnv, runAppAsIO)
import Servant.Server (Handler)

-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Colog.Message m

-- | Maing log action for the application. Prints message with some metadata to @stdout@.
mainLogAction :: MonadIO m => Severity -> LogAction m Colog.Message
mainLogAction severity =
  Colog.filterBySeverity severity Colog.msgSeverity Colog.richMessageAction

----------------------------------------------------------------------------
-- Application runners with runners
----------------------------------------------------------------------------

-- | Runs application as servant 'Handler'.
runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
  res <- liftIO $ runAppLogIO env app
  liftEither $ first toHttpError res

-- | Runs application like 'runAppAsIO' but also logs error.
runAppLogIO :: AppEnv -> App a -> IO (Either AppError a)
runAppLogIO env app = do
  appRes <- runAppAsIO env app
  logRes <- whenLeft (Right ()) appRes (logMPErrorIO env)
  pure $ appRes <* logRes

-- | Like 'runAppAsIO' but discards result.
runAppLogIO_ :: AppEnv -> App a -> IO ()
runAppLogIO_ env app = void $ runAppLogIO env app

----------------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------------

logMPErrorIO :: AppEnv -> AppError -> IO (Either AppError ())
logMPErrorIO env err = do
  -- Don’t log redirects. They are no real errors.
  if isRedirect $ appErrorType err
    then runAppAsIO env noLog
    else runAppAsIO env $ log E $ show err
  where
    noLog :: App ()
    noLog = return ()
