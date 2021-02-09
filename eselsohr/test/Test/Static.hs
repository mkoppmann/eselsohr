module Test.Static
  ( staticSpecs,
  )
where

import Lib.App (AppEnv)
import Lib.App.Env ()
import Lib.Web.Controller.Frontend (startpage)
import Test.Assert (succeeds)
import Test.Common (joinSpecs)
import Test.Hspec (Spec, describe, it)

staticSpecs :: AppEnv -> Spec
staticSpecs =
  joinSpecs
    "Static Resources"
    [startpageSpec]

startpageSpec :: AppEnv -> Spec
startpageSpec env = describe "Startpage Handler" $ do
  it "should return HTML" $
    env & succeeds startpage
