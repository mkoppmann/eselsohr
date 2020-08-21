module Lib.Effect.Resource.Article
  ( RWArticle,
    QueryArticle (..),
    CommandArticle (..),
  )
where

import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple.QQ (sql)
import Lib.App (App, WithError)
import Lib.Core.Article (Article, ArticleState (Archived, Unread))
import Lib.Core.Collection (Collection)
import Lib.Core.Id (Id)
import Lib.Db.Wrapper (WithDb, asSingleRow, collectionToAnyId, execute, executeNamed, queryNamed, query_)

class Monad m => QueryArticle m where
  getArticle :: Id Collection -> Id Article -> m Article
  getArticles :: Id Collection -> m [Article]
  getArchivedArticles :: Id Collection -> m [Article]
  getUnreadArticles :: Id Collection -> m [Article]

class Monad m => CommandArticle m where
  insertArticle :: Id Collection -> Article -> m ()
  changeArticleTitle :: Id Collection -> Id Article -> LText -> m ()
  archiveArticle :: Id Collection -> Id Article -> m ()
  unreadArticle :: Id Collection -> Id Article -> m ()
  deleteArticle :: Id Collection -> Id Article -> m ()

instance QueryArticle App where
  getArticle = getArticleDb
  getArticles = getArticlesDb
  getArchivedArticles = getArchivedArticlesDb
  getUnreadArticles = getUnreadArticlesDb

instance CommandArticle App where
  insertArticle = insertArticleDb
  changeArticleTitle = changeArticleTitleDb
  archiveArticle = archiveArticleDb
  unreadArticle = unreadArticleDb
  deleteArticle = deleteArticleDb

type RWArticle m = (QueryArticle m, CommandArticle m)

getArticleDb :: (WithDb env m, WithError m) => Id Collection -> Id Article -> m Article
getArticleDb cId aId =
  asSingleRow $
    queryNamed
      (collectionToAnyId cId)
      [sql|
        SELECT *
        FROM articles
        WHERE id = :aId
      |]
      [":aId" := aId]

getArticlesDb :: (WithDb env m) => Id Collection -> m [Article]
getArticlesDb cId =
  query_
    (collectionToAnyId cId)
    [sql|
      SELECT *
      FROM articles
    |]

getArchivedArticlesDb :: (WithDb env m) => Id Collection -> m [Article]
getArchivedArticlesDb cId = getFilteredArticlesDb cId Archived

getUnreadArticlesDb :: (WithDb env m) => Id Collection -> m [Article]
getUnreadArticlesDb cId = getFilteredArticlesDb cId Unread

getFilteredArticlesDb :: (WithDb env m) => Id Collection -> ArticleState -> m [Article]
getFilteredArticlesDb cId aState =
  queryNamed
    (collectionToAnyId cId)
    [sql|
        SELECT *
        FROM articles
        WHERE state = :aState
      |]
    [":aState" := aState]

insertArticleDb :: (WithDb env m) => Id Collection -> Article -> m ()
insertArticleDb cId article =
  execute
    (collectionToAnyId cId)
    [sql|
      INSERT INTO articles (id, title, url, state, created_at)
      VALUES (?,?,?,?,?)
    |]
    article

changeArticleTitleDb :: (WithDb env m) => Id Collection -> Id Article -> LText -> m ()
changeArticleTitleDb cId aId aTitle =
  executeNamed
    (collectionToAnyId cId)
    [sql|
      UPDATE articles
      SET title = :aTitle
      WHERE id = :aId
    |]
    [":aTitle" := aTitle, ":aId" := aId]

archiveArticleDb :: (WithDb env m) => Id Collection -> Id Article -> m ()
archiveArticleDb cId aId = setArticleState cId aId Archived

unreadArticleDb :: (WithDb env m) => Id Collection -> Id Article -> m ()
unreadArticleDb cId aId = setArticleState cId aId Unread

deleteArticleDb :: (WithDb env m) => Id Collection -> Id Article -> m ()
deleteArticleDb cId aId =
  executeNamed
    (collectionToAnyId cId)
    [sql|
      DELETE
      FROM articles
      WHERE id = :aId
    |]
    [":aId" := aId]

setArticleState :: (WithDb env m) => Id Collection -> Id Article -> ArticleState -> m ()
setArticleState cId aId aState =
  executeNamed
    (collectionToAnyId cId)
    [sql|
      UPDATE articles
      SET state = :aState
      WHERE id = :aId
    |]
    [":aId" := aId, ":aState" := aState]
