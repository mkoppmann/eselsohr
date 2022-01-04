module Lib.Domain.Repo.ArticleList
  ( ArticleListAction(..)
  , ArticleListRepo(..)
  , save
  ) where

import           Lib.Domain.Article             ( Article )
import           Lib.Domain.Authorization       ( ChangeStatePerm
                                                , ChangeTitlePerm
                                                , CreateArticlesPerm
                                                , DeleteArticlePerm
                                                )
import           Lib.Domain.Collection          ( Collection )
import           Lib.Domain.Id                  ( Id )
import           Lib.Domain.NonEmptyText        ( NonEmptyText )

data ArticleListAction
  = AddArticle !CreateArticlesPerm !(Id Article) !Article
  | ChangeArticleTitle !ChangeTitlePerm !NonEmptyText
  | MarkArticleAsUnread !ChangeStatePerm
  | MarkArticleAsRead !ChangeStatePerm
  | RemoveArticle !DeleteArticlePerm

class (Monad m) => ArticleListRepo m where
  nextId :: m (Id Article)
  saveAll :: Id Collection -> Seq ArticleListAction -> m ()

save :: (ArticleListRepo m) => Id Collection -> ArticleListAction -> m ()
save colId = saveAll colId . one
