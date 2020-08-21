{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Core.Uri
  ( Uri (..),
    render,
  )
where

import Control.Monad.Catch (MonadThrow, throwM)
import Database.SQLite.Simple (ResultError (ConversionFailed), SQLData (SQLText))
import Database.SQLite.Simple.FromField (FromField, fromField, returnError)
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField, toField)
import Servant.API (FromHttpApiData, parseUrlPiece)
import qualified Text.URI as U

newtype Uri = Uri {unUri :: U.URI}
  deriving (Eq) via U.URI

instance MonadThrow Ok where
  throwM = fail . show

instance FromField Uri where
  fromField _field@(Field (SQLText t) _) = U.mkURI t >>= (Ok . Uri)
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance ToField Uri where
  toField = SQLText . U.render . unUri

instance FromHttpApiData Uri where
  parseUrlPiece = either (Left . toText . displayException) (Right . Uri) . U.mkURI

render :: Uri -> LText
render = toLText . U.render . unUri
