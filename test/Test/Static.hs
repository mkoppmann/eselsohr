module Test.Static
  ( staticSpecs
  ) where

import           Test.Assert                    ( succeeds )
import           Test.Common                    ( joinSpecs )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )

import           Lib.Infra.Monad                ( AppEnv )
import           Lib.Ui.Web.Controller.Static   ( startpage )

staticSpecs :: AppEnv -> Spec
staticSpecs = joinSpecs "Static Resources" [startpageSpec]

startpageSpec :: AppEnv -> Spec
startpageSpec env = describe "Startpage Handler" $ do
  it "should return HTML" $ env & succeeds startpage
