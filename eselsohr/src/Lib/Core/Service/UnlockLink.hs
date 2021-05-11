module Lib.Core.Service.UnlockLink
  ( getUnlockLinks
  , createUnlockLink
  , deleteUnlockLink
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as Text
import           Data.Time                      ( UTCTime )
import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( AuthAction
                                                , Capability(..)
                                                , ExpirationDate(..)
                                                , Id
                                                , Reference(..)
                                                , Resource
                                                , Revocable
                                                , canViewArticles
                                                , defaultArticlesRef
                                                , mkAccesstoken
                                                )
import           Lib.Core.Effect                ( ContextState(..)
                                                , MonadTime(..)
                                                , WriteState(..)
                                                )
import qualified Lib.Core.Effect.Repository    as R
import           Lib.Core.Service.Util          ( getResId )
import           Relude.Extra                   ( secondF )

getUnlockLinks
  :: (MonadTime m, WithError m)
  => ContextState
  -> AuthAction
  -> m (Seq (Capability, Revocable))
getUnlockLinks ctxState authAction = do
  let resId = getResId ctxState
      sRes  = csResource ctxState

  currTime <- getCurrentTime
  allCaps  <- R.getAllCap sRes authAction

  pure
    . secondF (toRevocable resId)
    . Seq.sortBy expDateCmp
    . fmap swap
    . Seq.fromList
    . Map.toList
    $ Map.filter (filterF currTime) allCaps

 where
  expDateCmp :: (Capability, b) -> (Capability, b1) -> Ordering
  expDateCmp (cap1, _) (cap2, _) = compare cap1 cap2

  filterF :: UTCTime -> Capability -> Bool
  filterF currTime cap = isViewArticles cap && isStillValid currTime cap

  isViewArticles :: Capability -> Bool
  isViewArticles = canViewArticles . objectRef

  isStillValid :: UTCTime -> Capability -> Bool
  isStillValid currTime cap = case capExpirationDate cap of
    Nothing      -> True
    Just expDate -> unExpirationDate expDate > currTime

  toRevocable :: Id Resource -> Id Capability -> Revocable
  toRevocable resId capId = (capId, mkAccesstoken $ Reference resId capId)

createUnlockLink
  :: (WriteState m, WithError m)
  => ContextState
  -> Maybe Text
  -> Maybe ExpirationDate
  -> AuthAction
  -> m ()
createUnlockLink ctxState petname mExpDate authAct = do
  let resId          = getResId ctxState
      checkedPetname = checkForEmptyPetname petname
      cap            = Capability defaultArticlesRef checkedPetname mExpDate
  R.commit resId . one =<< R.insertCap authAct cap

deleteUnlockLink
  :: (WriteState m, WithError m) => ContextState -> AuthAction -> m ()
deleteUnlockLink ctxState authAct = do
  let resId = getResId ctxState
  R.commit resId . one =<< R.deleteCap authAct

checkForEmptyPetname :: Maybe Text -> Maybe Text
checkForEmptyPetname Nothing = Nothing
checkForEmptyPetname (Just pName) | Text.null pName = Nothing
                                  | otherwise       = Just pName
