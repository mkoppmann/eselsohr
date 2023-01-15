module Lib.Domain.CapabilityList
    ( CapabilityList
    , lookup
    , addUnlockLink
    , removeUnlockLink
    , addShareUnlockLinks
    , removeShareUnlockLinks
    , addShareArticleList
    , removeShareArticleList
    , addShareArticle
    , removeShareArticle
    , removeExpiredCapabilities
    , mkCapabilityList
    , fromMap
    , toMap
    , capabilityAlreadyExists
    ) where

import Data.Map.Strict qualified as Map

import Data.Time.Clock (UTCTime)

import Lib.Domain.Capability qualified as Cap

import Lib.Domain.Authorization
    ( CreateUnlockLinksPerm
    , DeleteUnlockLinksPerm
    , ShareArticleListPerm
    , ShareArticlePerm
    , ShareUnlockLinksPerm
    )
import Lib.Domain.Capability (Capability)
import Lib.Domain.Error
    ( AppErrorType
    , notFound
    , serverError
    )
import Lib.Domain.Id (Id)

newtype CapabilityList = CapabilityList (Map (Id Capability) Capability)
    deriving (Eq, Show) via (Map (Id Capability) Capability)

type AddCapability a = a -> Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList

type RemoveCapability a = a -> Id Capability -> CapabilityList -> CapabilityList

lookup :: Id Capability -> CapabilityList -> Either AppErrorType Capability
lookup capId = maybeToRight notFound . Map.lookup capId . coerce

addUnlockLink :: AddCapability CreateUnlockLinksPerm
addUnlockLink _perm = addCapability

{- | Removes the 'Capability' with the 'Id' contained in the 'DeleteUnlockLinksPerm'
from the 'CapabilityList'. This function does not fail, if the article was already
deleted, because delete is idempotent.
-}
removeUnlockLink :: RemoveCapability DeleteUnlockLinksPerm
removeUnlockLink _perm = removeCapability

removeExpiredCapabilities :: DeleteUnlockLinksPerm -> UTCTime -> CapabilityList -> CapabilityList
removeExpiredCapabilities _perm curTime = wrapMap $ Map.filter unexpiredCap
  where
    unexpiredCap :: Capability -> Bool
    unexpiredCap cap = case cap.expirationDate of
        Nothing -> True
        Just expDate -> expDate > curTime

addShareUnlockLinks :: AddCapability ShareUnlockLinksPerm
addShareUnlockLinks _perm = addCapability

{- | Removes the 'Capability' with the 'Id' contained in the 'ShareUnlockLinksPerm'
from the 'CapabilityList'. This function does not fail, if the article was already
deleted, because delete is idempotent.
-}
removeShareUnlockLinks :: RemoveCapability ShareUnlockLinksPerm
removeShareUnlockLinks _perm = removeCapability

addShareArticleList :: AddCapability ShareArticleListPerm
addShareArticleList _perm = addCapability

{- | Removes the 'Capability' with the 'Id' contained in the 'ShareArticleListPerm'
from the 'CapabilityList'. This function does not fail, if the article was already
deleted, because delete is idempotent.
-}
removeShareArticleList :: RemoveCapability ShareArticleListPerm
removeShareArticleList _perm = removeCapability

addShareArticle :: AddCapability ShareArticlePerm
addShareArticle _perm = addCapability

{- | Removes the 'Capability' with the 'Id' contained in the 'ShareArticlePerm'
from the 'CapabilityList'. This function does not fail, if the article was already
deleted, because delete is idempotent.
-}
removeShareArticle :: RemoveCapability ShareArticlePerm
removeShareArticle _perm = removeCapability

addCapability :: Id Capability -> Capability -> CapabilityList -> Either AppErrorType CapabilityList
addCapability capId cap capList = case lookup capId capList of
    Left _notFound -> pure $ wrapMap (Map.insert capId cap) capList
    Right _cap -> Left capabilityAlreadyExists

removeCapability :: Id Capability -> CapabilityList -> CapabilityList
removeCapability capId = wrapMap $ Map.delete capId

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

mkCapabilityList :: CapabilityList
mkCapabilityList = CapabilityList Map.empty

fromMap :: Map (Id Capability) Capability -> CapabilityList
fromMap = coerce

toMap :: CapabilityList -> Map (Id Capability) Capability
toMap = coerce

capabilityAlreadyExists :: AppErrorType
capabilityAlreadyExists = serverError "A capability with this id was already added. Please try again."

type Update a = (a -> a)

wrapMap :: Update (Map (Id Capability) Capability) -> CapabilityList -> CapabilityList
wrapMap f = coerce . f . coerce
