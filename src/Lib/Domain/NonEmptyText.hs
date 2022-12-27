module Lib.Domain.NonEmptyText
    ( NonEmptyText
    , fromText
    ) where

import qualified Data.Text as T

import Lib.Domain.Error
    ( AppErrorType
    , invalid
    )

newtype NonEmptyText = NonEmptyText Text
    deriving (Eq, Show) via Text

instance ToText NonEmptyText where
    toText = coerce

fromText :: Text -> Text -> Either AppErrorType NonEmptyText
fromText text errorMessage
    | T.null text = Left $ invalid errorMessage
    | otherwise = Right $ NonEmptyText text
