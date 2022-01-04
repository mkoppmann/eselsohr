module Lib.Domain.ArticleList
  ( ArticleList
  , addArticle
  , changeArticleTitle
  , markArticleAsUnread
  , markArticleAsRead
  , removeArticle
  , mkArticleList
  , fromMap
  , toMap
  ) where

import qualified Data.Map.Strict                                     as Map

import qualified Lib.Domain.Article                                  as Article
import qualified Lib.Domain.Authorization                            as Authz

import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.Authorization                             ( ChangeStatePerm
                                                                      , ChangeTitlePerm
                                                                      , CreateArticlesPerm
                                                                      , DeleteArticlePerm
                                                                      )
import           Lib.Domain.Error                                     ( AppErrorType
                                                                      , notFound
                                                                      , serverError
                                                                      )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.NonEmptyText                              ( NonEmptyText )

newtype ArticleList = ArticleList (Map (Id Article) Article)

addArticle :: CreateArticlesPerm -> Id Article -> Article -> ArticleList -> Either AppErrorType ArticleList
addArticle _perm artId art artList = case lookup artId artList of
  Left  _notFound -> pure $ wrapMap (Map.insert artId art) artList
  Right _art      -> Left $ serverError "An article with this id was already added. Please try again."

changeArticleTitle :: ChangeTitlePerm -> NonEmptyText -> ArticleList -> Either AppErrorType ArticleList
changeArticleTitle perm newTitle artList = do
  let artId = Authz.changeTitleId perm
  art <- lookup artId artList
  let newArt = Article.changeTitle newTitle art
  pure $ wrapMap (Map.insert artId newArt) artList

markArticleAsUnread :: ChangeStatePerm -> ArticleList -> Either AppErrorType ArticleList
markArticleAsUnread perm artList = do
  let artId = Authz.changeStateId perm
  art <- lookup artId artList
  let newArt = Article.markAsUnread art
  pure $ wrapMap (Map.insert artId newArt) artList

markArticleAsRead :: ChangeStatePerm -> ArticleList -> Either AppErrorType ArticleList
markArticleAsRead perm artList = do
  let artId = Authz.changeStateId perm
  art <- lookup artId artList
  let newArt = Article.markAsRead art
  pure $ wrapMap (Map.insert artId newArt) artList

{- | Removes the 'Article' with the 'Id' contained in the 'DeleteArticlePerm'
from the 'ArticleList'. This function does not fail, if the article was already
deleted, because delete is idempotent.
 -}
removeArticle :: DeleteArticlePerm -> ArticleList -> ArticleList
removeArticle perm = wrapMap . Map.delete $ Authz.deleteArticleId perm

------------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------------

mkArticleList :: ArticleList
mkArticleList = ArticleList Map.empty

fromMap :: Map (Id Article) Article -> ArticleList
fromMap = coerce

toMap :: ArticleList -> Map (Id Article) Article
toMap = coerce

lookup :: Id Article -> ArticleList -> Either AppErrorType Article
lookup artId = maybeToRight notFound . Map.lookup artId . coerce

type Update a = (a -> a)

wrapMap :: Update (Map (Id Article) Article) -> ArticleList -> ArticleList
wrapMap f = coerce . f . coerce
