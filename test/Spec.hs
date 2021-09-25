module Main where

import           Control.Exception              ( bracket )
import           Hedgehog                       ( Group(..)
                                                , checkParallel
                                                )
import           Lib                            ( mkAppEnv )
import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , runAppLogIO_
                                                )
import           Lib.Config                     ( Config(..)
                                                , loadConfig
                                                )
import qualified Lib.Init                      as Init
import           System.IO                      ( hSetEncoding
                                                , utf8
                                                )
import           System.IO.Temp                 ( createTempDirectory )
import           Test.Common                    ( joinSpecs )
import           Test.Core.Domain.Accesstoken   ( accToUrlRoundtrip
                                                , refToAccRoundtrip
                                                )
import           Test.Hspec                     ( Spec
                                                , hspec
                                                )
import           Test.Hspec.Core.Spec           ( sequential )
import           Test.Static                    ( staticSpecs )
import           UnliftIO.Directory             ( getTemporaryDirectory
                                                , removeDirectoryRecursive
                                                )

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
               (\Env {..} -> removeDirectoryRecursive envDataFolder)
               runTests
 where
  setTmpDir :: Config -> IO Config
  setTmpDir conf = do
    tmpDir <- getTemporaryDirectory
    fp     <- createTempDirectory tmpDir "eselsohr_test"
    pure $ conf { confDataFolder = fp }

  runTests :: AppEnv -> IO ()
  runTests env@Env {..} = do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    runAppLogIO_ env $ Init.datafolder envDataFolder
    Init.useRestrictedHttpManager

    -- run all tests
    hspec $ hspecTests env
    ifM (checkParallel hedgehogTests) exitSuccess exitFailure
