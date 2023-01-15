module Lib.Domain.Repo.ArticleList
    ( ArticleListAction
    , ArticleListRepo (..)
    , save
    , addArticle
    , changeArticleTitle
    , markArticleAsUnread
    , markArticleAsRead
    , removeArticle
    ) where

import Lib.Domain.ArticleList qualified as ArtList

import Lib.Domain.Article (Article)
import Lib.Domain.ArticleList (ArticleList)
import Lib.Domain.Authorization
    ( ChangeStatePerm
    , ChangeTitlePerm
    , CreateArticlesPerm
    , DeleteArticlePerm
    )
import Lib.Domain.Collection (Collection)
import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Id (Id)
import Lib.Domain.NonEmptyText (NonEmptyText)

type ArticleListAction = ArticleList -> Either AppErrorType ArticleList

class (Monad m) => ArticleListRepo m where
    loadAll :: Id Collection -> m ArticleList
    nextId :: m (Id Article)
    saveAll :: Id Collection -> Seq ArticleListAction -> m ()

save :: (ArticleListRepo m) => Id Collection -> ArticleListAction -> m ()
save colId = saveAll colId . one

addArticle :: CreateArticlesPerm -> Id Article -> Article -> ArticleListAction
addArticle = ArtList.addArticle

changeArticleTitle :: ChangeTitlePerm -> NonEmptyText -> ArticleListAction
changeArticleTitle = ArtList.changeArticleTitle

markArticleAsUnread :: ChangeStatePerm -> ArticleListAction
markArticleAsUnread = ArtList.markArticleAsUnread

markArticleAsRead :: ChangeStatePerm -> ArticleListAction
markArticleAsRead = ArtList.markArticleAsRead

removeArticle :: DeleteArticlePerm -> ArticleListAction
removeArticle perm = pure . ArtList.removeArticle perm
