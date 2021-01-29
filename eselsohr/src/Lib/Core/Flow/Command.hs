module Lib.Core.Flow.Command
  ( deleteAction,
    patchAction,
    postAction,
  )
where

import Lib.App.Error (WithError)
import qualified Lib.Core.Action.Command as Action
import Lib.Core.Domain.Article (Article)
import Lib.Core.Domain.Capability (Action (..), CommandAction (..), DeleteAction (..), PatchAction (..), PostAction (..))
import Lib.Core.Domain.Context (Context (..))
import qualified Lib.Core.Domain.Context as Context
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Domain.ExpirationDate (ExpirationDate)
import Lib.Core.Domain.Uri (Uri)
import Lib.Core.Effect.Random (MonadRandom)
import Lib.Core.Effect.Repository (Persist, RWEntity)
import Lib.Core.Effect.Scraper (MonadScraper)
import Lib.Core.Effect.Time (MonadTime)

deleteAction :: (Persist m) => Context -> m ()
deleteAction ctx = case Entity.val $ Context.ctxAct ctx of
  Command cAction -> case cAction of
    Delete delAction -> case delAction of
      DeleteGetArticles capId -> Action.deleteGetArticles ctx capId
      DeleteArticle aId -> Action.deleteArticle ctx aId
    _nonDeleteAction -> pure ()
  _nonCommandAction -> pure ()

patchAction :: (RWEntity Article m) => Context -> Maybe Text -> m ()
patchAction ctx mArtTitle = case Entity.val $ Context.ctxAct ctx of
  Command cAction -> case cAction of
    Patch patAction -> case patAction of
      ChangeArticleTitle aId -> Action.changeArticleTitle ctx aId mArtTitle
      ArchiveArticle aId -> Action.archiveArticle ctx aId
      UnreadArticle aId -> Action.unreadArticle ctx aId
    _nonPatchAction -> pure ()
  _nonCommandAction -> pure ()

postAction ::
  (RWEntity Article m, MonadRandom m, MonadScraper m, MonadTime m, WithError m) =>
  Context ->
  Maybe Uri ->
  Maybe Text ->
  Maybe ExpirationDate ->
  m (Maybe Text)
postAction ctx mUri unlockPetname expDate =
  case Entity.val $ Context.ctxAct ctx of
    Command cAction -> case cAction of
      Post posAction -> case posAction of
        CreateArticle aId -> Action.createArticle ctx aId mUri >> pure Nothing
        CreateGetArticlesCap cgacActions ->
          Action.createGetArticlesCap ctx unlockPetname expDate cgacActions
            >> pure Nothing
      _nonPostAction -> pure Nothing
    _nonCommandAction -> pure Nothing
