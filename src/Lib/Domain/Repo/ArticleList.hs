module Lib.Domain.Repo.ArticleList
  ( ArticleListAction(..)
  , ArticleListRepo(..)
  , save
  , apply
  ) where

import qualified Lib.Domain.ArticleList                              as ArtList

import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.ArticleList                               ( ArticleList )
import           Lib.Domain.Authorization                             ( ChangeStatePerm
                                                                      , ChangeTitlePerm
                                                                      , CreateArticlesPerm
                                                                      , DeleteArticlePerm
                                                                      )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.NonEmptyText                              ( NonEmptyText )

data ArticleListAction
  = AddArticle !CreateArticlesPerm !(Id Article) !Article
  | ChangeArticleTitle !ChangeTitlePerm !NonEmptyText
  | MarkArticleAsUnread !ChangeStatePerm
  | MarkArticleAsRead !ChangeStatePerm
  | RemoveArticle !DeleteArticlePerm

class (Monad m) => ArticleListRepo m where
  loadAll :: Id Collection -> m ArticleList
  nextId :: m (Id Article)
  saveAll :: Id Collection -> Seq ArticleListAction -> m ()

save :: (ArticleListRepo m) => Id Collection -> ArticleListAction -> m ()
save colId = saveAll colId . one

apply :: ArticleList -> ArticleListAction -> Either AppErrorType ArticleList
apply artList = \case
  AddArticle perm artId art        -> ArtList.addArticle perm artId art artList
  ChangeArticleTitle perm newTitle -> ArtList.changeArticleTitle perm newTitle artList
  MarkArticleAsUnread perm         -> ArtList.markArticleAsUnread perm artList
  MarkArticleAsRead   perm         -> ArtList.markArticleAsRead perm artList
  RemoveArticle       perm         -> pure $ ArtList.removeArticle perm artList
