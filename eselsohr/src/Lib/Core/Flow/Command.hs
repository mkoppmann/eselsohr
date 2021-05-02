module Lib.Core.Flow.Command
  ( deleteAction
  , patchAction
  , postAction
  ) where

import           Lib.App                        ( pattern E
                                                , WithError
                                                , WithLog
                                                , log
                                                )
import qualified Lib.Core.Action.Command       as Action
import           Lib.Core.Domain                ( Action(..)
                                                , CommandAction(..)
                                                , DeleteAction(..)
                                                , ExpirationDate
                                                , PatchAction(..)
                                                , PostAction(..)
                                                , Uri
                                                )
import           Lib.Core.Effect                ( ContextState(..)
                                                , MonadRandom
                                                , MonadScraper
                                                , MonadTime
                                                , WriteState
                                                )

import qualified Lib.Core.Domain.Context       as Context
import qualified Lib.Core.Domain.Entity        as Entity


deleteAction :: (WriteState m, WithLog env m) => ContextState -> m ()
deleteAction ctx = case Entity.val . Context.ctxAct $ csContext ctx of
  Command cAction -> case cAction of
    Delete delAction -> case delAction of
      DeleteGetArticles capId -> Action.deleteGetArticles ctx capId
      DeleteArticle     aId   -> Action.deleteArticle ctx aId
    _nonDeleteAction -> log E "Got nonDeleteAction"
  _nonCommandAction -> log E "Got nonDeleteAction"

patchAction
  :: (WriteState m, WithLog env m) => ContextState -> Maybe Text -> m ()
patchAction ctx mArtTitle = case Entity.val . Context.ctxAct $ csContext ctx of
  Command cAction -> case cAction of
    Patch patAction -> case patAction of
      ChangeArticleTitle aId -> Action.changeArticleTitle ctx aId mArtTitle
      ArchiveArticle     aId -> Action.archiveArticle ctx aId
      UnreadArticle      aId -> Action.unreadArticle ctx aId
    _nonPatchAction -> log E "Got nonPatchAction"
  _nonCommandAction -> log E "Got nonPatchAction"

postAction
  :: ( WriteState m
     , MonadRandom m
     , MonadScraper m
     , MonadTime m
     , WithError m
     , WithLog env m
     )
  => ContextState
  -> Maybe Uri
  -> Maybe Text
  -> Maybe ExpirationDate
  -> m (Maybe Text)
postAction ctx mUri unlockPetname expDate =
  case Entity.val . Context.ctxAct $ csContext ctx of
    Command cAction -> case cAction of
      Post posAction -> case posAction of
        CreateArticle aId -> Action.createArticle ctx aId mUri >> pure Nothing
        CreateGetArticlesCap cgacActions ->
          Action.createGetArticlesCap ctx unlockPetname expDate cgacActions
            >> pure Nothing
      _nonPostAction -> log E "Got nonPostAction" >> pure Nothing
    _nonCommandAction -> log E "Got nonPostAction" >> pure Nothing
