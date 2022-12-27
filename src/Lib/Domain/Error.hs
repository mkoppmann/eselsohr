module Lib.Domain.Error
    ( AppErrorType (..)
    , IError (..)

      -- * Error checks
    , isServerError
    , isInvalid
    , isRedirect

      -- * Internal error helpers
    , notFound
    , notAuthorized
    , serverError
    , invalid
    , missingParameter
    , storeError
    , limitError
    , redirect303
    , redirect307
    ) where

-- | App errors type.
newtype AppErrorType = InternalError IError
    deriving (Show, Eq) via IError

{- | The internal errors that can be thrown. These errors are meant to be
 handled within the application and cover exceptional circumstances/coding
 errors.
-}
data IError
    = -- | General not found.
      NotFound
    | -- | Some exceptional circumstance has happened stop execution and return.
      -- Optional text to provide some context in server logs.
      ServerError !Text
    | -- | A required permission level was not met. Optional text to provide some
      -- context.
      NotAuthorized !Text
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
isRedirect (InternalError (Redirect303 _)) = True
isRedirect (InternalError (Redirect307 _)) = True
isRedirect _ = False

----------------------------------------------------------------------------
-- Internal Error helpers
----------------------------------------------------------------------------

notFound :: AppErrorType
notFound = InternalError NotFound

serverError :: Text -> AppErrorType
serverError = InternalError . ServerError

notAuthorized :: Text -> AppErrorType
notAuthorized = InternalError . NotAuthorized

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
