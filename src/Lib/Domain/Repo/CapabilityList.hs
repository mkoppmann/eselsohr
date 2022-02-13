module Lib.Domain.Repo.CapabilityList
  ( CapabilityListAction
  , CapabilityListRepo(..)
  , save
  , addUnlockLink
  , removeUnlockLink
  , addShareUnlockLinks
  , removeShareUnlockLinks
  , addShareArticleList
  , removeShareArticleList
  , addShareArticle
  , removeShareArticle
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

type CapabilityListAction = CapabilityList -> Either AppErrorType CapabilityList

class (Monad m) => CapabilityListRepo m where
  loadAll :: Id Collection -> m CapabilityList
  nextId :: m (Id Capability)
  saveAll :: Id Collection -> Seq CapabilityListAction -> m ()

save :: (CapabilityListRepo m) => Id Collection -> CapabilityListAction -> m ()
save colId = saveAll colId . one

addUnlockLink :: CreateUnlockLinksPerm -> Id Capability -> Capability -> CapabilityListAction
addUnlockLink = CapList.addUnlockLink

removeUnlockLink :: DeleteUnlockLinksPerm -> Id Capability -> CapabilityListAction
removeUnlockLink perm capId = pure . CapList.removeUnlockLink perm capId

addShareUnlockLinks :: ShareUnlockLinksPerm -> Id Capability -> Capability -> CapabilityListAction
addShareUnlockLinks = CapList.addShareUnlockLinks

removeShareUnlockLinks :: ShareUnlockLinksPerm -> Id Capability -> CapabilityListAction
removeShareUnlockLinks perm capId = pure . CapList.removeShareUnlockLinks perm capId

addShareArticleList :: ShareArticleListPerm -> Id Capability -> Capability -> CapabilityListAction
addShareArticleList = CapList.addShareArticleList

removeShareArticleList :: ShareArticleListPerm -> Id Capability -> CapabilityListAction
removeShareArticleList perm capId = pure . CapList.removeShareArticleList perm capId

addShareArticle :: ShareArticlePerm -> Id Capability -> Capability -> CapabilityListAction
addShareArticle = CapList.addShareArticle

removeShareArticle :: ShareArticlePerm -> Id Capability -> CapabilityListAction
removeShareArticle perm capId = pure . CapList.removeShareArticle perm capId
