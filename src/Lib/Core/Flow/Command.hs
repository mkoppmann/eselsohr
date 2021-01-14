module Lib.Core.Flow.Command
  ( deleteAction,
    patchAction,
    postAction,
  )
where

import Data.Time (UTCTime)
import Lib.App.Error (WithError)
import Lib.App.Log (WithLog, log, pattern D)
import qualified Lib.Core.Action.Command as Action
import Lib.Core.Domain (Action (..), Article, CommandAction (..), Context (..), DeleteAction (..), PatchAction (..), PostAction (..), Uri)
import qualified Lib.Core.Domain.Context as Context
import qualified Lib.Core.Domain.Entity as Entity
import Lib.Core.Domain.ExpirationDate (ExpirationDate)
import Lib.Core.Effect (MonadRandom, MonadScraper, MonadTime, RWEntity, WriteEntity)

deleteAction ::
  (WriteEntity Article m, WithLog env m) =>
  Context ->
  m ()
deleteAction ctx = case Entity.val $ Context.ctxAct ctx of
  Command cAction -> case cAction of
    Delete delAction -> case delAction of
      DeleteGetArticles capId -> Action.deleteGetArticles ctx capId
      DeleteArticle aId -> Action.deleteArticle ctx aId
      DeleteSharedAction actId -> Action.deleteSharedAction ctx actId
    _nonDeleteAction -> pure ()
  _nonCommandAction -> pure ()

patchAction ::
  (RWEntity Article m) =>
  Context ->
  Maybe Text ->
  m ()
patchAction ctx mArtTitle = case Entity.val $ Context.ctxAct ctx of
  Command cAction -> case cAction of
    Patch patAction -> case patAction of
      ChangeArticleTitle aId -> Action.changeArticleTitle ctx aId mArtTitle
      ArchiveArticle aId -> Action.archiveArticle ctx aId
      UnreadArticle aId -> Action.unreadArticle ctx aId
    _nonPatchAction -> pure ()
  _nonCommandAction -> pure ()

postAction ::
  ( RWEntity Article m,
    MonadRandom m,
    MonadScraper m,
    MonadTime m,
    WithError m,
    WithLog env m
  ) =>
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
        CreateResource -> Just <$> Action.createResource ctx
        CreateGetArticlesCap cgacActions ->
          Action.createGetArticlesCap ctx unlockPetname expDate cgacActions >> pure Nothing
        ShareAction actId -> Action.shareAction ctx actId >> pure Nothing
      _nonPostAction -> pure Nothing
    _nonCommandAction -> pure Nothing
