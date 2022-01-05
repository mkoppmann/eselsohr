module Test.Infra.Repo.ArticleList
  ( testSaveAll
  ) where

import qualified Lib.Infra.Persistence.Model.ArticleList             as ArtListPm
import qualified Lib.Infra.Persistence.Model.Collection              as ColPm
import qualified Lib.Infra.Repo.ArticleList                          as ArtListRepo

import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.ArticleList                          ( ArticleListAction )
import           Lib.Infra.Error                                      ( WithError
                                                                      , throwOnError
                                                                      )
import           Test.App.Env                                         ( CollectionState
                                                                      , Has
                                                                      , grab
                                                                      )

testSaveAll
  :: (MonadReader env m, Has CollectionState env, MonadIO m, WithError m)
  => Id Collection
  -> Seq ArticleListAction
  -> m ()
testSaveAll _colId updates = do
  collectionRef <- grab @CollectionState
  collection    <- readIORef collectionRef
  articles      <- throwOnError . ArtListPm.toDomain $ ColPm.articleList collection
  newArticles   <- fmap ArtListPm.fromDomain . throwOnError $ foldlM ArtListRepo.apply articles updates
  writeIORef collectionRef $ collection { ColPm.articleList = newArticles }
