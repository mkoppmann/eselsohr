module Lib.Infra.Error
    ( AppError (..)
    , AppException (..)
    , WithError
    , throwError
    , toHttpError

      -- * Error throwing helpers
    , throwOnNothing
    , throwOnNothingM
    , notFoundOnNothing
    , notFoundOnNothingM
    , throwOnError
    , throwOnErrorM
    , redirectTo
    ) where

import qualified Control.Monad.Except as E
import qualified Servant.Server as Servant
    ( ServerError
    )

import Control.Monad.Except (MonadError)
import GHC.Stack (SrcLoc (..))
import Servant
    ( err303
    , err307
    , err400
    , err403
    , err404
    , err413
    , err417
    , err500
    , errBody
    , errHeaders
    )

import Lib.Domain.Error
    ( AppErrorType (..)
    , IError (..)
    , notFound
    , redirect303
    )

-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)
{-# INLINE throwError #-}

newtype SourcePosition = SourcePosition Text
    deriving (Show, Eq) via Text

{- | Display 'CallStack' as 'SourcePosition' in a format:
 @Module.function#line_number@.
-}
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        [] -> "<unknown loc>"
        [(name, loc)] -> showLoc name loc
        (_, loc) : (callerName, _) : _rest -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} = toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

{- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
 'AppError' as 'Exception'.
-}
newtype AppException = AppException
    { unAppException :: AppError
    }
    deriving (Show) via AppError
    deriving anyclass (Exception)

-- | 'AppErrorType' with the corresponding 'CallStack'.
data AppError = AppError
    { appErrorCallStack :: !SourcePosition
    , appErrorType :: !AppErrorType
    }
    deriving stock (Show, Eq)

-- | Map 'AppError' into a HTTP error code.
toHttpError :: AppError -> Servant.ServerError
toHttpError (AppError _callStack errorType) = case errorType of
    InternalError err -> case err of
        NotFound -> err404
        ServerError msg -> err500{errBody = encodeUtf8 msg}
        NotAuthorized msg -> err403{errBody = encodeUtf8 msg}
        MissingParameter name -> err400{errBody = "Parameter not found: " <> encodeUtf8 name}
        Invalid msg -> err417{errBody = encodeUtf8 msg}
        StoreError e -> err500{errBody = encodeUtf8 e}
        LimitError -> err413{errBody = "Request is over the limits"}
        Redirect303 linkTo -> err303{errHeaders = [("Location", linkTo)]}
        Redirect307 linkTo -> err307{errHeaders = [("Location", linkTo)]}

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

{- | Extract the value from a 'Maybe', throwing the given 'AppError' if
 the value does not exist
-}
throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err value = withFrozenCallStack $ maybe (throwError err) pure value

{- | Extract the value from a 'Maybe' in @m@, throwing the given 'AppError' if
 the value does not exist
-}
throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack $ throwOnNothing err =<< action

{- | Similar to 'throwOnNothing' but throws a 'NotFound' if the value does not
 exist
-}
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing value = withFrozenCallStack $ throwOnNothing notFound value

{- | Similar to 'throwOnNothingM' but throws a 'NotFound' if the value does not
 exist
-}
notFoundOnNothingM :: WithError m => m (Maybe a) -> m a
notFoundOnNothingM value = withFrozenCallStack $ throwOnNothingM notFound value

-- | Extract the value from an 'Either', throwing the embedded 'AppErrorType' if the value is Left
throwOnError :: WithError m => Either AppErrorType a -> m a
throwOnError value = withFrozenCallStack $ either throwError pure value

-- | Extract the value from an 'Either' in @m@, throwing the embedded 'AppErrorType' if the value is Left
throwOnErrorM :: WithError m => m (Either AppErrorType a) -> m a
throwOnErrorM action = withFrozenCallStack $ either throwError pure =<< action

-- | Redirects to the given URL
redirectTo :: WithError m => Text -> m a
redirectTo = throwError . redirect303
