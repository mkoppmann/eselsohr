module Lib.Infra.Repo.ArticleList
  ( nextId
  , saveAll
  ) where

import qualified Lib.App.Port                                        as Port
import qualified Lib.Domain.Repo.ArticleList                         as Repo
import qualified Lib.Infra.Persistence.File                          as File
import qualified Lib.Infra.Persistence.Model.ArticleList             as ArtListPm
import qualified Lib.Infra.Persistence.Model.Collection              as ColPm

import           Lib.App.Port                                         ( MonadRandom )
import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Id                                        ( Id )
import           Lib.Domain.Repo.ArticleList                          ( ArticleListAction )
import           Lib.Infra.Error                                      ( WithError
                                                                      , throwOnError
                                                                      )
import           Lib.Infra.Persistence.File                           ( WithFile )
import           Lib.Infra.Persistence.Model.Collection               ( CollectionPm )
import           Lib.Infra.Persistence.Queue                          ( WithQueue
                                                                      , commit
                                                                      )

nextId :: (MonadRandom m) => m (Id Article)
nextId = Port.getRandomId

saveAll :: (WithError m, WithFile env m, WithQueue env m) => Id Collection -> Seq ArticleListAction -> m ()
saveAll colId updates = do
  let action = File.save updater colId
  commit colId action
 where
  updater :: WithError m => CollectionPm -> m CollectionPm
  updater colPm = do
    artList    <- throwOnError . ArtListPm.toDomain $ ColPm.articleList colPm
    newArtList <- throwOnError $ foldlM Repo.apply artList updates
    pure $ colPm { ColPm.articleList = ArtListPm.fromDomain newArtList }
