module Lib.Core.Domain.Article
  ( Article(..)
  , ArticleState(..)
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( toTitle )
import           Data.Time                      ( UTCTime )
import           Lib.Core.Domain.Entity         ( Entity )
import           Lib.Core.Domain.Uri            ( Uri )
import           Web.HttpApiData                ( FromHttpApiData(..) )

data ArticleState
  = Unread
  | Archived
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Serialise, FromJSON, ToJSON)

instance FromHttpApiData ArticleState where
  parseUrlPiece = readEither . toString . toTitle

data Article = Article
  { title    :: !Text
  , uri      :: !Uri
  , state    :: !ArticleState
  , creation :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Serialise, FromJSON, ToJSON)

instance Serialise (Entity Article)

instance Ord Article where
  compare a b = compare (creation a) (creation b)
