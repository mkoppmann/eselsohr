module Lib.Infra.Repo.ArticleList
  ( nextId
  , saveAll
  ) where

import qualified Lib.App.Port                                        as Port
import qualified Lib.Domain.ArticleList                              as ArtList
import qualified Lib.Domain.Repo.ArticleList                         as Repo
import qualified Lib.Infra.Persistence.File                          as File
import qualified Lib.Infra.Persistence.Model.ArticleList             as ArtListPm
import qualified Lib.Infra.Persistence.Model.Collection              as ColPm

import           Lib.App.Env                                          ( Environment
                                                                      , Has
                                                                      , grab
                                                                      )
import           Lib.App.Port                                         ( MonadRandom )
import           Lib.Domain.Article                                   ( Article )
import           Lib.Domain.ArticleList                               ( ArticleList )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Error                                     ( AppErrorType )
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

type WithAppEnv env m = (MonadReader env m, Has Environment env)

nextId :: (MonadRandom m) => m (Id Article)
nextId = Port.getRandomId

saveAll
  :: (WithAppEnv env m, WithError m, WithFile env m, WithQueue env m) => Id Collection -> Seq ArticleListAction -> m ()
saveAll colId updates = do
  let action = File.save updater colId
  commit colId action
 where
  updater :: (WithAppEnv env m, WithError m) => CollectionPm -> m CollectionPm
  updater colPm = do
    appEnv     <- grab @Environment
    artList    <- throwOnError . ArtListPm.toDomain appEnv $ ColPm.articleList colPm
    newArtList <- throwOnError $ foldlM apply artList updates
    pure $ colPm { ColPm.articleList = ArtListPm.fromDomain newArtList }

apply :: ArticleList -> ArticleListAction -> Either AppErrorType ArticleList
apply artList = \case
  Repo.AddArticle perm artId art        -> ArtList.addArticle perm artId art artList
  Repo.ChangeArticleTitle perm newTitle -> ArtList.changeArticleTitle perm newTitle artList
  Repo.MarkArticleAsUnread perm         -> ArtList.markArticleAsUnread perm artList
  Repo.MarkArticleAsRead   perm         -> ArtList.markArticleAsRead perm artList
  Repo.RemoveArticle       perm         -> pure $ ArtList.removeArticle perm artList
