module Lib.Core.Service.Collection
  ( createCollection
  ) where

import           Lib.Core.Domain                ( Accesstoken
                                                , Capability(..)
                                                , Id
                                                , Reference(..)
                                                , Resource
                                                , defaultOverviewRef
                                                , mkAccesstoken
                                                )
import           Lib.Core.Effect                ( MonadRandom(..)
                                                , WriteState(..)
                                                )
import qualified Lib.Core.Effect.Repository    as R

createCollection :: (WriteState m, MonadRandom m) => m Accesstoken
createCollection = createAccess =<< createFile
 where
  createFile :: (WriteState m, MonadRandom m) => m (Id Resource)
  createFile = getRandomId >>= \resId -> R.init resId >> pure resId

  createAccess :: (WriteState m, MonadRandom m) => Id Resource -> m Accesstoken
  createAccess resId = do
    let cap = Capability defaultOverviewRef Nothing Nothing
    capId <- getRandomId
    let capSe = R.noAuthInsertCap capId cap
    R.commit resId $ one capSe
    pure . mkAccesstoken $ Reference resId capId
