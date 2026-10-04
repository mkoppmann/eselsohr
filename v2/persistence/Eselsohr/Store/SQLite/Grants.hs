{- |
Module      : Eselsohr.Store.SQLite.Grants
Description : Protected description of the presented grant

'describeGrant' tells the holder of a presented bearer what its grant permits.
It runs in a deferred read transaction, loads the bearer's grant and ancestry, samples the clock once, and requires only a valid chain, so expired, revoked, and unknown bearers are denied like any other operation.

'checkDescription' produces a t'DescriptionRequest' that holds the acting grant it authorized, and 'readDescription' turns that request into a t'GrantView' of permissions, scope, and expiration.
The view omits the grant's identifier, parent, depth, and petname, because the petname is the issuer's note about the holder and the ancestry describes other grants.
The request's transaction parameter prevents its use in another transaction.
-}
module Eselsohr.Store.SQLite.Grants
    ( describeGrant
    , DescriptionRequest
    , checkDescription
    , readDescription
    ) where

import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection)
import Eselsohr.Authorization (Operation (..))
import Eselsohr.Capability (GrantSnapshot (..))
import Eselsohr.Store (GrantView (..))
import Eselsohr.Store.SQLite.Bearer (Bearer)
import Eselsohr.Store.SQLite.Internal.Authority (authorizeActor)
import Eselsohr.Store.SQLite.Internal.Transaction

type role DescriptionRequest nominal
newtype DescriptionRequest tx = DescriptionRequest GrantSnapshot

describeGrant :: Connection -> Bearer -> IO UTCTime -> IO (Either StoreError GrantView)
describeGrant connection bearer clock =
    runReadTransaction connection bearer clock $ checkDescription >>= readDescription

checkDescription :: ReadTx tx (DescriptionRequest tx)
checkDescription = DescriptionRequest <$> authorizeActor DescribeOwnGrant

-- | The view of the acting grant that the request's transaction authorized.
readDescription :: DescriptionRequest tx -> ReadTx tx GrantView
readDescription (DescriptionRequest actor) =
    pure (GrantView (grantPermissions actor) (grantScope actor) (grantExpiresAt actor))
