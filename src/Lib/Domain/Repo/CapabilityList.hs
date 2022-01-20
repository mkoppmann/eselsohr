module Lib.Domain.Repo.CapabilityList
  ( CapabilityListAction(..)
  , CapabilityListRepo(..)
  , save
  , apply
  ) where

import qualified Lib.Domain.CapabilityList                           as CapList

import           Lib.Domain.Authorization                             ( CreateUnlockLinksPerm
                                                                      , DeleteUnlockLinksPerm
                                                                      , ShareArticleListPerm
                                                                      , ShareArticlePerm
                                                                      , ShareUnlockLinksPerm
                                                                      )
import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.CapabilityList                            ( CapabilityList )
import           Lib.Domain.Collection                                ( Collection )
import           Lib.Domain.Error                                     ( AppErrorType )
import           Lib.Domain.Id                                        ( Id )

data CapabilityListAction
  = AddUnlockLink !CreateUnlockLinksPerm !(Id Capability) !Capability
  | RemoveUnlockLink !DeleteUnlockLinksPerm !(Id Capability)
  | AddShareUnlockLinks !ShareUnlockLinksPerm !(Id Capability) !Capability
  | RemoveShareUnlockLinks !ShareUnlockLinksPerm !(Id Capability)
  | AddShareArticleList !ShareArticleListPerm !(Id Capability) !Capability
  | RemoveShareArticleList !ShareArticleListPerm !(Id Capability)
  | AddShareArticle !ShareArticlePerm !(Id Capability) !Capability
  | RemoveShareArticle !ShareArticlePerm !(Id Capability)

class (Monad m) => CapabilityListRepo m where
  loadAll :: Id Collection -> m CapabilityList
  nextId :: m (Id Capability)
  saveAll :: Id Collection -> Seq CapabilityListAction -> m ()

save :: (CapabilityListRepo m) => Id Collection -> CapabilityListAction -> m ()
save colId = saveAll colId . one

apply :: CapabilityList -> CapabilityListAction -> Either AppErrorType CapabilityList
apply capList = \case
  AddUnlockLink perm capId cap       -> CapList.addUnlockLink perm capId cap capList
  RemoveUnlockLink perm capId        -> pure $ CapList.removeUnlockLink perm capId capList
  AddShareUnlockLinks perm capId cap -> CapList.addShareUnlockLinks perm capId cap capList
  RemoveShareUnlockLinks perm capId  -> pure $ CapList.removeShareUnlockLinks perm capId capList
  AddShareArticleList perm capId cap -> CapList.addShareArticleList perm capId cap capList
  RemoveShareArticleList perm capId  -> pure $ CapList.removeShareArticleList perm capId capList
  AddShareArticle perm capId cap     -> CapList.addShareArticle perm capId cap capList
  RemoveShareArticle perm capId      -> pure $ CapList.removeShareArticle perm capId capList
