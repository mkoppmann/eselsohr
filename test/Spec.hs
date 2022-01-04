module Main where

import           Config                                               ( Config(..)
                                                                      , loadConfig
                                                                      )
import           Control.Exception                                    ( bracket )
import           Hedgehog                                             ( Group(..)
                                                                      , checkParallel
                                                                      )
import           System.IO                                            ( hSetEncoding
                                                                      , utf8
                                                                      )
import           System.IO.Temp                                       ( createTempDirectory )
import           Test.Common                                          ( joinSpecs )
import           Test.Hspec                                           ( Spec
                                                                      , hspec
                                                                      )
import           Test.Hspec.Core.Spec                                 ( sequential )
import           Test.Static                                          ( staticSpecs )
import           Test.Web.Accesstoken                                 ( accToUrlRoundtrip
                                                                      , refToAccRoundtrip
                                                                      )
import           UnliftIO.Directory                                   ( getTemporaryDirectory
                                                                      , removeDirectoryRecursive
                                                                      )

import qualified Init
import qualified Lib.App.Env                                         as Env

import           Lib                                                  ( mkAppEnv )
import           Lib.Infra.Log                                        ( runAppLogIO_ )
import           Lib.Infra.Monad                                      ( AppEnv )

hspecTests :: AppEnv -> Spec
hspecTests = sequential . joinSpecs "Eselsohr" [staticSpecs]

hedgehogTests :: Group
hedgehogTests = Group
  "Roundtrip Properties"
  [ refToAccRoundtrip `named` "toReference . mkAccesstoken ≡ True"
  , accToUrlRoundtrip `named` "parseUrlPiece . toUrlPiece ≡ True"
  ]
 where
  named :: a -> b -> (b, a)
  named = flip (,)

main :: IO ()
main = bracket (loadConfig Nothing >>= setTmpDir >>= mkAppEnv)
               (\Env.Env {..} -> removeDirectoryRecursive dataFolder)
               runTests
 where
  setTmpDir :: Config -> IO Config
  setTmpDir conf = do
    tmpDir <- getTemporaryDirectory
    fp     <- createTempDirectory tmpDir "eselsohr_test"
    pure $ conf { confDataFolder = fp }

  runTests :: AppEnv -> IO ()
  runTests env@Env.Env {..} = do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    runAppLogIO_ env $ Init.datafolder dataFolder
    Init.useRestrictedHttpManager

    -- run all tests
    hspec $ hspecTests env
    ifM (checkParallel hedgehogTests) exitSuccess exitFailure
