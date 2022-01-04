module Lib.Domain.Repo.CapabilityList
  ( CapabilityListAction(..)
  , CapabilityListRepo(..)
  , save
  ) where

import           Lib.Domain.Authorization                             ( CreateUnlockLinksPerm
                                                                      , DeleteUnlockLinksPerm
                                                                      , ShareArticleListPerm
                                                                      , ShareArticlePerm
                                                                      , ShareUnlockLinksPerm
                                                                      )
import           Lib.Domain.Capability                                ( Capability )
import           Lib.Domain.Collection                                ( Collection )
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
  nextId :: m (Id Capability)
  saveAll :: Id Collection -> Seq CapabilityListAction -> m ()

save :: (CapabilityListRepo m) => Id Collection -> CapabilityListAction -> m ()
save colId = saveAll colId . one
