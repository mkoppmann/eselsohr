module Lib.Ui.Web.Page.ViewModel.Article
    ( ArticleVm (..)
    , ArticleStateVm (..)
    , fromDomain
    ) where

import Data.Time.Clock (UTCTime)
import Prelude hiding
    ( id
    , state
    )

import qualified Lib.Domain.Article as Article

import Lib.Domain.Article
    ( Article
    , ArticleState
    )

import Lib.Domain.Id (Id)
import Lib.Domain.Uri as Uri

data ArticleVm = ArticleVm
    { id :: !(Id Article)
    , title :: !Text
    , uri :: !Text
    , uriHost :: !(Maybe Text)
    , state :: !ArticleStateVm
    , creation :: !UTCTime
    }

data ArticleStateVm = Unread | Read
    deriving stock (Show, Read)

fromDomain :: Article -> ArticleVm
fromDomain art = do
    let id = art.id
        title = toText art.title
        uri = toText art.uri
        uriHost = Uri.getHostname art.uri
        state = articleStateVmFromDomain art.state
        creation = art.creation
    ArticleVm{..}

articleStateVmFromDomain :: ArticleState -> ArticleStateVm
articleStateVmFromDomain Article.Unread = Unread
articleStateVmFromDomain Article.Read = Read
