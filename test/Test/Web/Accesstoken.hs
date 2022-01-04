module Test.Web.Accesstoken
  ( refToAccRoundtrip
  , accToUrlRoundtrip
  ) where

import           Hedgehog                       ( (===)
                                                , Property
                                                , PropertyT
                                                , property
                                                , tripping
                                                )
import           Web.HttpApiData                ( parseUrlPiece
                                                , toUrlPiece
                                                )

import           Lib.Infra.Adapter.Random       ( getRandomId )
import           Lib.Ui.Web.Dto.Accesstoken     ( Reference(..)
                                                , mkAccesstoken
                                                , toReference
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
