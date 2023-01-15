module Lib.Infra.Persistence.Model.ArticleList
    ( ArticleListPm
    , fromDomain
    , toDomain
    , migrate
    ) where

import Data.Map.Strict qualified as Map

import Lib.Domain.ArticleList qualified as Domain
import Lib.Infra.Persistence.Model.Article qualified as Article

import Lib.Domain.Article (Article)
import Lib.Domain.ArticleList (ArticleList)
import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Id (Id)
import Lib.Infra.Persistence.Model.Article (ArticlePm)
import Lib.Infra.Persistence.Model.Shared
    ( modelListFromDomain
    , modelListToDomain
    )

type ArticleListPm = Map (Id Article) ArticlePm

------------------------------------------------------------------------
-- Mapper
------------------------------------------------------------------------

fromDomain :: ArticleList -> ArticleListPm
fromDomain = Map.fromList . modelListFromDomain Article.fromDomain . Domain.toMap

toDomain :: ArticleListPm -> Either AppErrorType ArticleList
toDomain = Right . Domain.fromMap . Map.fromList <=< modelListToDomain Article.toDomain

------------------------------------------------------------------------
-- Migration
------------------------------------------------------------------------

migrate :: ArticleListPm -> ArticleListPm
migrate = fmap Article.migrate
