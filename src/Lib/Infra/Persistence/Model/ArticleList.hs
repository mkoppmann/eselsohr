module Lib.Infra.Persistence.Model.ArticleList
  ( ArticleListPm
  , fromDomain
  , toDomain
  ) where

import qualified Data.Map.Strict               as Map

import qualified Lib.Domain.ArticleList        as Domain
import qualified Lib.Infra.Persistence.Model.Article
                                               as Article

import           Lib.Domain.Article             ( Article )
import           Lib.Domain.ArticleList         ( ArticleList )
import           Lib.Domain.Error               ( AppErrorType )
import           Lib.Domain.Id                  ( Id )
import           Lib.Infra.Persistence.Model.Article
                                                ( ArticlePm )
import           Lib.Infra.Persistence.Model.Shared
                                                ( modelListFromDomain
                                                , modelListToDomain
                                                )

type ArticleListPm = Map (Id Article) ArticlePm

fromDomain :: ArticleList -> ArticleListPm
fromDomain =
  Map.fromList . modelListFromDomain Article.fromDomain . Domain.toMap

toDomain :: ArticleListPm -> Either AppErrorType ArticleList
toDomain =
  Right . Domain.fromMap . Map.fromList <=< modelListToDomain Article.toDomain
