module Test.Core.Domain.Accesstoken
  ( refToAccRoundtrip
  , accToUrlRoundtrip
  ) where

import           Hedgehog                       ( (===)
                                                , Property
                                                , PropertyT
                                                , property
                                                , tripping
                                                )
import           Lib.Core.Domain.Accesstoken    ( Reference(..)
                                                , mkAccesstoken
                                                , toReference
                                                )
import           Lib.Impl.Random                ( getRandomId )
import           Web.HttpApiData                ( parseUrlPiece
                                                , toUrlPiece
                                                )

refToAccRoundtrip :: Property
refToAccRoundtrip = property $ do
  ref <- genReference
  toReference (mkAccesstoken ref) === ref

accToUrlRoundtrip :: Property
accToUrlRoundtrip = property $ do
  acc <- mkAccesstoken <$> genReference
  tripping acc toUrlPiece parseUrlPiece

genReference :: (MonadIO m) => PropertyT m Reference
genReference = Reference <$> getRandomId <*> getRandomId
