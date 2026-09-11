module Eselsohr.Capability
    ( -- * Permissions and scopes
      Permission (..)
    , Permissions
    , Scope (..)
    , permissionsSubsetOf
    , scopeSubsetOf

      -- * Delegation
    , GrantSnapshot (..)
    , AttenuationError (..)
    , maxDelegationDepth
    , attenuates

      -- * Grant chains
    , ValidChain
    , ChainError (..)
    , validateChain
    , validateGrantStructure
    , chainGrants
    , chainCheckedAt
    ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Eselsohr.Id (ArticleId, CapabilityId, CollectionId)

------------------------------------------------------------------------
-- Permissions and scopes
------------------------------------------------------------------------

data Permission
    = ReadArticles
    | AddArticles
    | ChangeTitles
    | ChangeReadState
    | DeleteArticles
    | Delegate
    | InspectLinks
    | RevokeLinks
    deriving stock (Eq, Ord, Enum, Bounded, Show)

-- Permission ordering serves set storage; inclusion uses permissionsSubsetOf.
type Permissions = Set Permission

-- | The collection or article targeted by a grant or operation.
data Scope
    = WholeCollection CollectionId
    | OneArticle CollectionId ArticleId
    deriving stock (Eq, Show)

-- | Check whether every candidate permission belongs to the parent set.
permissionsSubsetOf :: Permissions -> Permissions -> Bool
permissionsSubsetOf = Set.isSubsetOf

-- | Check a candidate scope against a containing parent in the same collection.
scopeSubsetOf :: Scope -> Scope -> Bool
scopeSubsetOf (WholeCollection candidate) (WholeCollection parent) = candidate == parent
scopeSubsetOf (OneArticle candidate _) (WholeCollection parent) = candidate == parent
scopeSubsetOf (WholeCollection _) (OneArticle _ _) = False
scopeSubsetOf (OneArticle candidate article) (OneArticle parent parentArticle) =
    candidate == parent && article == parentArticle

------------------------------------------------------------------------
-- Delegation
------------------------------------------------------------------------

-- | Unvalidated grant fields supplied to the policy checks.
data GrantSnapshot = GrantSnapshot
    { grantId :: CapabilityId
    , grantParentId :: Maybe CapabilityId
    , grantScope :: Scope
    , grantPermissions :: Permissions
    , grantExpiresAt :: Maybe UTCTime
    , grantDepth :: Int
    }
    deriving stock (Eq, Show)

data AttenuationError
    = InvalidParent
    | ParentExpired
    | DelegateRequired
    | DepthExhausted
    | InvalidChild
    | ChildExpired
    | ParentMismatch
    | DepthMismatch
    | ScopeWidened
    | PermissionsIncreased
    | ChildOutlivesParent
    deriving stock (Eq, Show)

maxDelegationDepth :: Int
maxDelegationDepth = 4

-- | Check one parent-child edge at the supplied time, not the complete ancestry.
attenuates :: UTCTime -> GrantSnapshot -> GrantSnapshot -> Either AttenuationError ()
attenuates instant = attenuatesWith (CheckExpiry instant)

data ExpiryCheck = CheckExpiry UTCTime | AllowExpired

isExpired :: ExpiryCheck -> Maybe UTCTime -> Bool
isExpired (CheckExpiry instant) deadline = maybe False (<= instant) deadline
isExpired AllowExpired _ = False

attenuatesWith :: ExpiryCheck -> GrantSnapshot -> GrantSnapshot -> Either AttenuationError ()
attenuatesWith expiry parent child
    | not (validParentShape parent) = Left InvalidParent
    | isExpired expiry (grantExpiresAt parent) = Left ParentExpired
    | Delegate `Set.notMember` grantPermissions parent = Left DelegateRequired
    | grantDepth parent == maxDelegationDepth = Left DepthExhausted
    | not (validChildShape child) = Left InvalidChild
    | isExpired expiry (grantExpiresAt child) = Left ChildExpired
    | grantParentId child /= Just (grantId parent) || grantId child == grantId parent = Left ParentMismatch
    | grantDepth child /= grantDepth parent + 1 = Left DepthMismatch
    | not (scopeSubsetOf (grantScope child) (grantScope parent)) = Left ScopeWidened
    | not (permissionsSubsetOf (grantPermissions child) (grantPermissions parent)) = Left PermissionsIncreased
    | Just parentDeadline <- grantExpiresAt parent
    , Just childDeadline <- grantExpiresAt child
    , childDeadline > parentDeadline =
        Left ChildOutlivesParent
    | otherwise = Right ()

validParentShape :: GrantSnapshot -> Bool
validParentShape grant = case grantParentId grant of
    Nothing -> validRootShape grant
    Just _ -> validChildShape grant

validRootShape :: GrantSnapshot -> Bool
validRootShape grant = case (grantParentId grant, grantScope grant) of
    (Nothing, WholeCollection _) ->
        grantDepth grant == 0
            && isNothing (grantExpiresAt grant)
            && grantPermissions grant == Set.fromList [minBound .. maxBound]
    _ -> False

validChildShape :: GrantSnapshot -> Bool
validChildShape grant = case (grantParentId grant, grantExpiresAt grant) of
    (Just parent, Just _) ->
        parent /= grantId grant
            && grantDepth grant >= 1
            && grantDepth grant <= maxDelegationDepth
    _ -> False

------------------------------------------------------------------------
-- Grant chains
------------------------------------------------------------------------

-- | A complete policy snapshot checked at the supplied time, not current bearer authority.
data ValidChain = ValidChain UTCTime (NonEmpty GrantSnapshot)
    deriving stock (Eq, Show)

data ChainError
    = EmptyChain
    | ChainTooLong
    | RequestedGrantMismatch
    | CollectionMismatch
    | RepeatedGrantId
    | MissingRoot
    | InvalidRoot
    | InvalidEdge AttenuationError
    deriving stock (Eq, Show)

-- | Return the checked snapshots in leaf-to-root order.
chainGrants :: ValidChain -> NonEmpty GrantSnapshot
chainGrants (ValidChain _ grants) = grants

chainCheckedAt :: ValidChain -> UTCTime
chainCheckedAt (ValidChain instant _) = instant

-- | Validate the requested grant and every supplied ancestor without repairing the path.
validateChain :: UTCTime -> CollectionId -> CapabilityId -> [GrantSnapshot] -> Either ChainError ValidChain
validateChain instant collection requested snapshots =
    ValidChain instant <$> checkGrantChain (CheckExpiry instant) collection requested snapshots

-- | Check ancestry and attenuation while allowing expired grants, without establishing current authority.
validateGrantStructure :: CollectionId -> CapabilityId -> [GrantSnapshot] -> Either ChainError (NonEmpty GrantSnapshot)
validateGrantStructure = checkGrantChain AllowExpired

checkGrantChain :: ExpiryCheck -> CollectionId -> CapabilityId -> [GrantSnapshot] -> Either ChainError (NonEmpty GrantSnapshot)
checkGrantChain expiry collection requested snapshots =
    case splitAt (maxDelegationDepth + 1) snapshots of
        (_, _ : _) -> Left ChainTooLong
        ([], []) -> Left EmptyChain
        (leaf : ancestors, []) -> checkBounded (leaf :| ancestors)
  where
    checkBounded grants@(leaf :| _)
        | grantId leaf /= requested = Left RequestedGrantMismatch
        | not (all (\grant -> scopeSubsetOf (grantScope grant) (WholeCollection collection)) grants) = Left CollectionMismatch
        | Set.size (Set.fromList (NonEmpty.toList identifiers)) /= NonEmpty.length grants = Left RepeatedGrantId
        | isJust (grantParentId root) = Left MissingRoot
        | not (validRootShape root) = Left InvalidRoot
        | otherwise = do
            checkEdges (reverse (NonEmpty.toList grants))
            Right grants
      where
        root = NonEmpty.last grants
        identifiers = grantId <$> grants
    checkEdges (parent : child : rest) = do
        first InvalidEdge (attenuatesWith expiry parent child)
        checkEdges (child : rest)
    checkEdges _ = Right ()
