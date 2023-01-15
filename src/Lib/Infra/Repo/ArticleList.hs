module Lib.Infra.Repo.ArticleList
    ( loadAll
    , nextId
    , saveAll
    ) where

import Lib.App.Port qualified as Port
import Lib.Infra.Persistence.File qualified as File
import Lib.Infra.Persistence.Model.ArticleList qualified as ArtListPm
import Lib.Infra.Persistence.Model.Collection qualified as ColPm

import Lib.App.Port (MonadRandom)
import Lib.Domain.Article (Article)
import Lib.Domain.ArticleList (ArticleList)
import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)
import Lib.Domain.Repo.ArticleList (ArticleListAction)
import Lib.Infra.Error
    ( WithError
    , throwOnError
    )
import Lib.Infra.Persistence.File (WithFile)
import Lib.Infra.Persistence.Model.Collection (CollectionPm)
import Lib.Infra.Persistence.Queue
    ( WithQueue
    , commit
    )

loadAll :: (WithError m, WithFile env m) => Id Collection -> m ArticleList
loadAll colId = articlesFromCollection =<< File.load colId id

nextId :: (MonadRandom m) => m (Id Article)
nextId = Port.getRandomId

saveAll :: (WithError m, WithFile env m, WithQueue env m) => Id Collection -> Seq ArticleListAction -> m ()
saveAll colId updates = commit colId action
  where
    action :: (WithError m, WithFile env m) => m ()
    action = do
        collection <- File.load colId id
        articles <- articlesFromCollection collection
        newArticles <- throwOnError $ foldlM (&) articles updates
        File.save colId $ ColPm.updateArticleList (ArtListPm.fromDomain newArticles) collection

articlesFromCollection :: WithError m => CollectionPm -> m ArticleList
articlesFromCollection = throwOnError . ArtListPm.toDomain . ColPm.getArticleList
