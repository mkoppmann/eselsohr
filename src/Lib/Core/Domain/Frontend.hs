module Lib.Core.Domain.Frontend
  ( ResourceOverviewAccess (..),
    ShowArticlesAccess (..),
    ShowArticleAccess (..),
  )
where

import Lib.Core.Domain.Accesstoken (Accesstoken)
import Lib.Core.Domain.Article (Article)

data ResourceOverviewAccess = ResourceOverviewAccess
  { roAccGetSharedActions :: !Accesstoken,
    roAccCreateGetArticlesCap :: !(Maybe Accesstoken)
  }

data ShowArticlesAccess = ShowArticlesAccess
  { sasAccGetSharedArticlesAction :: !Accesstoken,
    sasAccCreateArticle :: !(Maybe Accesstoken),
    sasAccArticleAccs :: !(Seq (Article, ShowArticleAccess))
  }
  deriving stock (Show)

data ShowArticleAccess = ShowArticleAccess
  { saAccShowArticle :: !Accesstoken,
    saAccChangeArticleTitle :: !(Maybe Accesstoken),
    saAccArchiveArticle :: !(Maybe Accesstoken),
    saAccUnreadArticle :: !(Maybe Accesstoken),
    saAccDeleteArticle :: !(Maybe Accesstoken),
    saAccGetArticles :: !(Maybe Accesstoken)
  }
  deriving stock (Show)
