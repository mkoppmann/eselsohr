module Test.Infra.Repo.ArticleList
  ( testLoadAll
  , testSaveAll
  ) where

import qualified Lib.Domain.Repo.ArticleList                         as Repo
import qualified Lib.Infra.Persistence.Model.ArticleList             as ArtListPm
import qualified Lib.Infra.Persistence.Model.Collection              as ColPm

import           Lib.Domain.ArticleList                               ( ArticleList )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.ArticleList                          ( ArticleListAction )
import           Lib.Infra.Error                                      ( WithError
                                                                      , throwOnError
                                                                      )
import           Lib.Infra.Persistence.Model.Collection               ( CollectionPm )
import           Test.App.Env                                         ( CollectionState
                                                                      , Has
                                                                      , grab
                                                                      )

testLoadAll :: (MonadReader env m, Has CollectionState env, MonadIO m, WithError m) => Id Collection -> m ArticleList
testLoadAll _colId = articlesFromCollection =<< readIORef =<< grab @CollectionState

testSaveAll
  :: (MonadReader env m, Has CollectionState env, MonadIO m, WithError m)
  => Id Collection
  -> Seq ArticleListAction
  -> m ()
testSaveAll _colId updates = do
  collectionRef <- grab @CollectionState
  collection    <- readIORef collectionRef
  articles      <- articlesFromCollection collection
  newArticles   <- fmap ArtListPm.fromDomain . throwOnError $ foldlM Repo.apply articles updates
  writeIORef collectionRef $ collection { ColPm.articleList = newArticles }

articlesFromCollection :: WithError m => CollectionPm -> m ArticleList
articlesFromCollection = throwOnError . ArtListPm.toDomain . ColPm.articleList
