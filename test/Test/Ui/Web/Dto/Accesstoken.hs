module Test.Ui.Web.Dto.Accesstoken
  ( accesstokenProps
  ) where

import           Hedgehog                                             ( (===)
                                                                      , Group(Group)
                                                                      , Property
                                                                      , PropertyT
                                                                      , property
                                                                      , tripping
                                                                      )
import           Web.HttpApiData                                      ( parseUrlPiece
                                                                      , toUrlPiece
                                                                      )

import qualified Lib.Infra.Adapter.Random                            as Adapter
import qualified Lib.Ui.Dto.Accesstoken                          as Accesstoken

import           Lib.Ui.Dto.Accesstoken                           ( Reference )

accesstokenProps :: Group
accesstokenProps = Group
  "Lib.Ui.Web.Dto.Accesstoken"
  [ ("toReference . mkAccesstoken ≡ True", referenceToAccesstokenRoundtrip)
  , ("parseUrlPiece . toUrlPiece ≡ True" , accesstokenToUrlRoundtrip)
  ]

referenceToAccesstokenRoundtrip :: Property
referenceToAccesstokenRoundtrip = property $ do
  reference <- generateReference
  let accesstoken = Accesstoken.mkAccesstoken reference
  Accesstoken.toReference accesstoken === reference

accesstokenToUrlRoundtrip :: Property
accesstokenToUrlRoundtrip = property $ do
  acc <- Accesstoken.mkAccesstoken <$> generateReference
  tripping acc toUrlPiece parseUrlPiece

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

generateReference :: (MonadIO m) => PropertyT m Reference
generateReference = Accesstoken.Reference <$> Adapter.getRandomId <*> Adapter.getRandomId
