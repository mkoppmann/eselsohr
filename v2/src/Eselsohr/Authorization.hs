{- |
Module      : Eselsohr.Authorization
Description : Operation policy for grants and their targets

An t'Operation' identifies a requested action and its target.
'decide' revalidates the acting grant chain at the supplied time and checks the permissions and scope required by that operation.

Describing the acting grant to its holder requires only the revalidated chain and no permission.
Issuing a child requires validation of the extended grant chain.
Link inspection and revocation require the target's ancestry to agree with the acting grant and its ancestors, and permit management of structurally valid expired descendants.
All decisions use the grants and time supplied by the caller.
-}
module Eselsohr.Authorization
    ( Operation (..)
    , PolicyError (..)
    , decide
    ) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Eselsohr.Capability
import Eselsohr.Id (ArticleId, CapabilityId, CollectionId)

-- | A requested operation with the inputs needed for its policy check.
data Operation
    = DescribeOwnGrant
    | ReadArticlesIn Scope
    | AddArticle CollectionId
    | RenameArticle CollectionId ArticleId
    | ChangeArticleReadState CollectionId ArticleId
    | DeleteArticle CollectionId ArticleId
    | IssueChild GrantSnapshot
    | InspectLink CapabilityId [GrantSnapshot]
    | RevokeLink CapabilityId [GrantSnapshot]
    | DeleteCollection CollectionId
    deriving stock (Eq, Show)

data PolicyError
    = ActorChainRejected ChainError
    | ChildChainRejected ChainError
    | TargetChainRejected ChainError
    | OperationDenied
    deriving stock (Eq, Show)

-- | Revalidate the supplied grant chain at the given time and check permission for the requested operation.
decide :: UTCTime -> ValidChain -> Operation -> Either PolicyError ()
decide instant chain operation = do
    _ <- first ActorChainRejected $ validateChain instant collection (grantId actor) grants

    case operation of
        DescribeOwnGrant -> Right ()
        ReadArticlesIn scope -> require [ReadArticles] scope
        AddArticle target -> require [AddArticles] (WholeCollection target)
        RenameArticle target article -> require [ReadArticles, ChangeTitles] (OneArticle target article)
        ChangeArticleReadState target article -> require [ReadArticles, ChangeReadState] (OneArticle target article)
        DeleteArticle target article -> require [DeleteArticles] (OneArticle target article)
        IssueChild child -> do
            allow (has Delegate)
            void . first ChildChainRejected $ validateChain instant collection (grantId child) (child : grants)
        InspectLink target path -> manage InspectLinks True target path
        RevokeLink target path -> manage RevokeLinks False target path
        DeleteCollection target -> allow (owner && target == collection)
  where
    grants = NonEmpty.toList (chainGrants chain)
    actor = NonEmpty.head (chainGrants chain)
    collection = case grantScope actor of
        WholeCollection identifier -> identifier
        OneArticle identifier _ -> identifier
    owner = case grants of
        [_] -> True
        _ -> False
    has permission = permission `Set.member` grantPermissions actor
    require permissions scope = allow (all has permissions && scopeSubsetOf scope (grantScope actor))
    manage permission inspectRoot target path = do
        allow (has permission)
        checked <- first TargetChainRejected $ validateGrantStructure collection target path
        let (descendants, shared) = break ((== grantId actor) . grantId) (NonEmpty.toList checked)
        allow (shared == grants && (not (null descendants) || (inspectRoot && owner)))

allow :: Bool -> Either PolicyError ()
allow True = Right ()
allow False = Left OperationDenied
