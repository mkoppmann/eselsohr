{-# LANGUAGE DeriveAnyClass #-}

module Lib.App.Error
  ( AppError (..),
    AppErrorType,
    AppException (..),
    WithError,
    throwError,
    toHttpError,

    -- * Redirect hack
    redirect,

    -- * Error checks
    isServerError,
    isInvalid,
    isRedirect,

    -- * Internal error helpers
    notFound,
    serverError,
    invalid,
    missingParameter,
    dbError,
    limitError,

    -- * Error throwing helpers
    throwOnNothing,
    throwOnNothingM,
    notFoundOnNothing,
    notFoundOnNothingM,
  )
where

import Control.Monad.Except (MonadError)
import qualified Control.Monad.Except as E (throwError)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))
import Servant.Server (err303, err400, err404, err413, err417, err500, errBody, errHeaders)
import qualified Servant.Server as Servant (ServerError)

-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)
{-# INLINE throwError #-}

newtype SourcePosition = SourcePosition LText
  deriving (Show, Eq) via LText

-- | Display 'CallStack' as 'SourcePosition' in a format: @Module.function#line_number@.
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: LText
    showCallStack = case getCallStack cs of
      [] -> "<unknown loc>"
      [(name, loc)] -> showLoc name loc
      (_, loc) : (callerName, _) : _rest -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> LText
    showLoc name SrcLoc {..} =
      toLText srcLocModule <> "." <> toLText name <> "#" <> show srcLocStartLine

-- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
-- 'AppError' as 'Exception'.
newtype AppException = AppException
  { unAppException :: AppError
  }
  deriving (Show) via AppError
  deriving anyclass (Exception)

-- | 'AppErrorType' with the corresponding 'CallStack'.
data AppError = AppError
  { appErrorCallStack :: !SourcePosition,
    appErrorType :: !AppErrorType
  }
  deriving stock (Show, Eq)

-- | App errors type.
newtype AppErrorType = InternalError IError
  deriving (Show, Eq) via IError

-- | The internal errors that can be thrown. These errors are meant to be
-- handled within the application and cover exceptional circumstances/coding errors.
data IError
  = -- | General not found.
    NotFound
  | -- | Some exceptional circumstance has happened stop execution and return.
    --    Optional text to provide some context in server logs.
    ServerError !LText
  | -- | An expected parameter was not given by the client. Optional text to
    --    provide the parameter’s name.
    MissingParameter !LText
  | -- | Given inputs do not conform to the expected format or shape. Optional
    --    text to provide some context in server logs.
    Invalid !LText
  | -- | Data base specific errors.
    DbError !LText
  | -- | Limits on the multi-request are overflowed.
    LimitError
  | -- | Redirect to given URL. Not a real error but a hack currently required
    --    in Servant
    Redirect !ByteString
  deriving stock (Show, Eq)

{- instance Scotty.ScottyError IOException where
  stringError = show @LText -}

-- | Map 'AppError' into a HTTP error code.
toHttpError :: AppError -> Servant.ServerError
toHttpError (AppError _callStack errorType) = case errorType of
  InternalError err -> case err of
    NotFound -> err404
    ServerError msg -> err500 {errBody = encodeUtf8 msg}
    MissingParameter name -> err400 {errBody = "Parameter not found: " <> encodeUtf8 name}
    Invalid msg -> err417 {errBody = encodeUtf8 msg}
    DbError e -> err500 {errBody = encodeUtf8 e}
    LimitError -> err413 {errBody = "Request is over the limits"}
    Redirect linkTo -> err303 {errHeaders = [("Location", linkTo)]}

----------------------------------------------------------------------------
-- Error checks
----------------------------------------------------------------------------

isServerError :: AppErrorType -> Bool
isServerError (InternalError (ServerError _)) = True
isServerError _ = False

isInvalid :: AppErrorType -> Bool
isInvalid (InternalError (Invalid _)) = True
isInvalid _ = False

isRedirect :: AppErrorType -> Bool
isRedirect (InternalError (Redirect _)) = True
isRedirect _ = False

----------------------------------------------------------------------------
-- Internal Error helpers
----------------------------------------------------------------------------

notFound :: AppErrorType
notFound = InternalError NotFound

serverError :: LText -> AppErrorType
serverError = InternalError . ServerError

missingParameter :: LText -> AppErrorType
missingParameter = InternalError . MissingParameter

invalid :: LText -> AppErrorType
invalid = InternalError . Invalid

dbError :: LText -> AppErrorType
dbError = InternalError . DbError

limitError :: AppErrorType
limitError = InternalError LimitError

redirect :: LText -> AppErrorType
redirect = InternalError . Redirect . encodeUtf8

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure

-- | Extract the value from a 'Maybe' in @m@, throwing the given 'AppError' if
-- the value does not exist
throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ action >>= throwOnNothing err

-- | Similar to 'throwOnNothing' but throws a 'NotFound' if the value does not exist
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing = withFrozenCallStack . throwOnNothing notFound

-- | Similar to 'throwOnNothingM' but throws a 'NotFound' if the value does not exist
notFoundOnNothingM :: WithError m => m (Maybe a) -> m a
notFoundOnNothingM = withFrozenCallStack . throwOnNothingM notFound
