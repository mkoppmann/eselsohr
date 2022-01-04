module Lib.Ui.Web.Page.ViewModel.Article
  ( ArticleVm(..)
  , ArticleStateVm(..)
  , fromDomain
  ) where

import           Data.Time.Clock                                      ( UTCTime )
import           Prelude                                       hiding ( id
                                                                      , state
                                                                      )

import qualified Lib.Domain.Article                                  as Article

import           Lib.Domain.Article                                   ( Article
                                                                      , ArticleState
                                                                      )

import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Uri                                      as Uri

data ArticleVm = ArticleVm
  { id       :: !(Id Article)
  , title    :: !Text
  , uri      :: !Text
  , uriHost  :: !(Maybe Text)
  , state    :: !ArticleStateVm
  , creation :: !UTCTime
  }

data ArticleStateVm = Unread | Read
  deriving stock (Show, Read)

fromDomain :: Article -> ArticleVm
fromDomain art = do
  let id       = Article.id art
      title    = toText $ Article.title art
      uri      = toText $ Article.uri art
      uriHost  = Uri.getHostname $ Article.uri art
      state    = articleStateVmFromDomain $ Article.state art
      creation = Article.creation art
  ArticleVm { .. }

articleStateVmFromDomain :: ArticleState -> ArticleStateVm
articleStateVmFromDomain Article.Unread = Unread
articleStateVmFromDomain Article.Read   = Read
