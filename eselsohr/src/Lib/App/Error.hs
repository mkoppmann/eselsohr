module Lib.App.Error
  ( AppError(..)
  , AppErrorType
  , AppException(..)
  , WithError
  , throwError
  , toHttpError
  ,

  -- * Redirect hack
    redirect303
  , redirect307
  ,

  -- * Error checks
    isServerError
  , isInvalid
  , isRedirect
  ,

  -- * Internal error helpers
    notFound
  , serverError
  , invalid
  , missingParameter
  , storeError
  , limitError
  ,

  -- * Error throwing helpers
    throwOnNothing
  , throwOnNothingM
  , notFoundOnNothing
  , notFoundOnNothingM
  ) where

import           Control.Monad.Except           ( MonadError )
import qualified Control.Monad.Except          as E
                                                ( throwError )
import           GHC.Stack                      ( SrcLoc
                                                  ( SrcLoc
                                                  , srcLocModule
                                                  , srcLocStartLine
                                                  )
                                                )
import           Servant.Server                 ( err303
                                                , err307
                                                , err400
                                                , err404
                                                , err413
                                                , err417
                                                , err500
                                                , errBody
                                                , errHeaders
                                                )
import qualified Servant.Server                as Servant
                                                ( ServerError )

-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)
{-# INLINE throwError #-}

newtype SourcePosition = SourcePosition Text
  deriving (Show, Eq) via Text

-- | Display 'CallStack' as 'SourcePosition' in a format: @Module.function#line_number@.
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
 where
  showCallStack :: Text
  showCallStack = case getCallStack cs of
    [] -> "<unknown loc>"
    [(name, loc)] -> showLoc name loc
    (_, loc) : (callerName, _) : _rest -> showLoc callerName loc

  showLoc :: String -> SrcLoc -> Text
  showLoc name SrcLoc {..} =
    toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

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
  , appErrorType      :: !AppErrorType
  }
  deriving stock (Show, Eq)

-- | App errors type.
newtype AppErrorType = InternalError IError
  deriving (Show, Eq) via IError

{- | The internal errors that can be thrown. These errors are meant to be
 handled within the application and cover exceptional circumstances/coding errors.
-}
data IError
  = -- | General not found.
    NotFound
  | -- | Some exceptional circumstance has happened stop execution and return.
    -- Optional text to provide some context in server logs.
    ServerError !Text
  | -- | An expected parameter was not given by the client. Optional text to
    -- provide the parameter’s name.
    MissingParameter !Text
  | -- | Given inputs do not conform to the expected format or shape. Optional
    -- text to provide some context in server logs.
    Invalid !Text
  | -- | Repository specific errors.
    StoreError !Text
  | -- | Limits on the multi-request are overflowed.
    LimitError
  | -- | Redirect to given URL. Not a real error but a hack currently required
    -- in Servant. 303 means that the browser use get to fetch the page and also
    -- drop the body.
    Redirect303 !ByteString
  | -- | Redirect to given URL. Not a real error but a hack currently required
    -- in Servant. 307 means that the browser use the same method and body for
    -- the redirect.
    Redirect307 !ByteString
  deriving stock (Show, Eq)

-- | Map 'AppError' into a HTTP error code.
toHttpError :: AppError -> Servant.ServerError
toHttpError (AppError _callStack errorType) = case errorType of
  InternalError err -> case err of
    NotFound        -> err404
    ServerError msg -> err500 { errBody = encodeUtf8 msg }
    MissingParameter name ->
      err400 { errBody = "Parameter not found: " <> encodeUtf8 name }
    Invalid    msg     -> err417 { errBody = encodeUtf8 msg }
    StoreError e       -> err500 { errBody = encodeUtf8 e }
    LimitError         -> err413 { errBody = "Request is over the limits" }
    Redirect303 linkTo -> err303 { errHeaders = [("Location", linkTo)] }
    Redirect307 linkTo -> err307 { errHeaders = [("Location", linkTo)] }

----------------------------------------------------------------------------
-- Error checks
----------------------------------------------------------------------------

isServerError :: AppErrorType -> Bool
isServerError (InternalError (ServerError _)) = True
isServerError _                               = False

isInvalid :: AppErrorType -> Bool
isInvalid (InternalError (Invalid _)) = True
isInvalid _                           = False

isRedirect :: AppErrorType -> Bool
isRedirect (InternalError (Redirect303 _)) = True
isRedirect (InternalError (Redirect307 _)) = True
isRedirect _                               = False

----------------------------------------------------------------------------
-- Internal Error helpers
----------------------------------------------------------------------------

notFound :: AppErrorType
notFound = InternalError NotFound

serverError :: Text -> AppErrorType
serverError = InternalError . ServerError

missingParameter :: Text -> AppErrorType
missingParameter = InternalError . MissingParameter

invalid :: Text -> AppErrorType
invalid = InternalError . Invalid

storeError :: Text -> AppErrorType
storeError = InternalError . StoreError

limitError :: AppErrorType
limitError = InternalError LimitError

redirect303 :: Text -> AppErrorType
redirect303 = InternalError . Redirect303 . encodeUtf8

redirect307 :: Text -> AppErrorType
redirect307 = InternalError . Redirect307 . encodeUtf8

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

{- | Extract the value from a maybe, throwing the given 'AppError' if
 the value does not exist
-}
throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure

{- | Extract the value from a 'Maybe' in @m@, throwing the given 'AppError' if
 the value does not exist
-}
throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action =
  withFrozenCallStack $ action >>= throwOnNothing err

-- | Similar to 'throwOnNothing' but throws a 'NotFound' if the value does not exist
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing = withFrozenCallStack . throwOnNothing notFound

-- | Similar to 'throwOnNothingM' but throws a 'NotFound' if the value does not exist
notFoundOnNothingM :: WithError m => m (Maybe a) -> m a
notFoundOnNothingM = withFrozenCallStack . throwOnNothingM notFound
