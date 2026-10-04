{- |
Module      : Eselsohr.Store.SQLite.Internal.Authority
Description : Stored grant validation inside protected transactions

'authorize' resolves the transaction's bearer to a stored grant and loads that grant's ancestry within the delegation depth limit.
It checks that the chain ends at the sole stored root, then samples the transaction's clock and checks expiration and operation policy.
'authorizeActor' performs the same checks and also returns the acting grant, so an operation can describe the grant it has just authorized.
'authorizeAt' returns the acting grant together with the sampled time, so an operation can record the instant at which it was authorized without sampling the clock again.

The SQLite article operations use this helper before constructing their sealed requests.
The connection, bearer, and clock come from the current transaction.
-}
module Eselsohr.Store.SQLite.Internal.Authority
    ( authorize
    , authorizeActor
    , authorizeAt
    ) where

import Control.Monad (unless, void)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple qualified as SQL
import Eselsohr.Authorization (Operation, decide)
import Eselsohr.Capability
import Eselsohr.Id (CapabilityId)
import Eselsohr.Store.SQLite.Bearer (bearerCollection, lookupBearer)
import Eselsohr.Store.SQLite.Bearer qualified as Bearer
import Eselsohr.Store.SQLite.Codec
import Eselsohr.Store.SQLite.Internal.Transaction

authorize :: Operation -> Tx mode tx ()
authorize = void . authorizeAt

-- | Authorize the operation and return the acting grant at the start of the checked chain.
authorizeActor :: Operation -> Tx mode tx GrantSnapshot
authorizeActor = fmap fst . authorizeAt

-- | Authorize the operation and return the acting grant and the one time sample of the check.
authorizeAt :: Operation -> Tx mode tx (GrantSnapshot, UTCTime)
authorizeAt operation = do
    TxContext connection bearer clock <- askContext
    matched <- liftTxIO (lookupBearer connection bearer) >>= checked . first lookupError
    leaf <- checked (maybe (Left StoreDenied) (Right . storedGrant) matched)

    grants <- loadChain Set.empty maxDelegationDepth leaf
    roots <- liftTxIO (SQL.query_ connection "SELECT id FROM capability WHERE parent_id IS NULL LIMIT 2")
    rootId <- decoded . toCapabilityBytes . grantId $ NonEmpty.last grants
    checked $ unless (roots == [Only rootId]) (Left StoreCorrupt)

    instant <- liftTxIO clock
    chain <- checked . first (const StoreDenied) $ validateChain instant (bearerCollection bearer) (grantId leaf) (NonEmpty.toList grants)
    checked . first (const StoreDenied) $ decide instant chain operation
    pure (leaf, instant)

loadChain :: Set.Set CapabilityId -> Int -> GrantSnapshot -> Tx mode tx (NonEmpty GrantSnapshot)
loadChain seen remaining grant = do
    checked $ unless (grantId grant `Set.notMember` seen) (Left StoreDenied)
    rest <- case grantParentId grant of
        Nothing -> pure []
        Just parent -> do
            let visited = Set.insert (grantId grant) seen
            checked $ unless (remaining > 0 && parent `Set.notMember` visited) (Left StoreDenied)

            TxContext connection bearer _ <- askContext
            identifier <- decoded (toCapabilityBytes parent)
            rows <- liftTxIO (SQL.query connection "SELECT id, token_hash, parent_id, scope_article_id, expires_at, depth, petname FROM capability WHERE id = ? LIMIT 2" (Only identifier))
            row <- checked $ case rows of
                [] -> Left StoreDenied
                [value] -> Right value
                _ -> Left StoreCorrupt

            permissions <- liftTxIO (SQL.query connection "SELECT capability_id, permission FROM capability_permission WHERE capability_id = ? LIMIT 9" (Only identifier))
            ancestor <- decoded (fromGrantRows (bearerCollection bearer) row permissions)

            NonEmpty.toList <$> loadChain visited (remaining - 1) (storedGrant ancestor)
    pure (grant :| rest)

lookupError :: Bearer.LookupError -> StoreError
lookupError = \case
    Bearer.CorruptData -> StoreCorrupt
    Bearer.LookupBusy -> StoreBusy
    _ -> StoreFault
