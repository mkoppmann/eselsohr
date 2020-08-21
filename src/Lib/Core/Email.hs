module Lib.Core.Email
  ( Email,
    mkEmail,
    unEmail,
  )
where

import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)

newtype Email = Email LText
  deriving (Eq, Read, FromField, ToField) via LText

unEmail :: Email -> LText
unEmail (Email email) = email

mkEmail :: String -> Maybe Email
mkEmail email =
  if '@' `elem` email
    then pure . Email $ toLText email
    else Nothing
