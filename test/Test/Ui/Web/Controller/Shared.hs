module Test.Ui.Web.Controller.Shared
  ( matchPartial
  , withTestEnv
  , redirectLocation
  , accAsByteString
  , accFromPath
  , accFromResponse
  , linkAsByteString
  , postForm
  , postCreateCollection
  , postCreateUnlockLink
  , getOverviewPage
  ) where

import qualified Data.ByteString.Char8                               as B
import qualified Test.Hspec.Wai                                      as Test

import           Network.HTTP.Types                                   ( methodPost )
import           Network.HTTP.Types.Header                            ( hContentType
                                                                      , hLocation
                                                                      )
import           Network.Wai                                          ( Application )
import           Network.Wai.Handler.Warp                             ( runSettings )
import           Network.Wai.Test                                     ( SResponse(..) )
import           Servant.Links                                        ( Link
                                                                      , fieldLink
                                                                      )
import           System.FilePath                                      ( addTrailingPathSeparator )
import           System.IO                                            ( hSetEncoding
                                                                      , utf8
                                                                      )
import           System.IO.Temp                                       ( createTempDirectory )
import           Test.Hspec                                           ( ActionWith )
import           Test.Hspec.Wai                                       ( MatchBody(..)
                                                                      , WaiSession
                                                                      , request
                                                                      )
import           UnliftIO                                             ( async
                                                                      , bracket
                                                                      )
import           UnliftIO.Directory                                   ( getTemporaryDirectory
                                                                      , removeDirectoryRecursive
                                                                      )
import           Web.FormUrlEncoded                                   ( urlEncodeAsForm )
import           Web.HttpApiData                                      ( parseUrlPiece )

import qualified Config
import qualified Init
import qualified Lib.App.Env                                         as Env
import qualified Lib.Infra.Persistence.Server                        as Server
import qualified Lib.Ui.Server                                       as Server
import qualified Lib.Ui.Web.Route                                    as Route

import           Config                                               ( Config
                                                                      , loadConfig
                                                                      )
import           Lib                                                  ( mkAppEnv
                                                                      , warpSettings
                                                                      )
import           Lib.Infra.Log                                        ( runAppLogIO_ )
import           Lib.Infra.Monad                                      ( AppEnv )
import           Lib.Ui.Dto.Accesstoken                               ( Accesstoken )
import           Lib.Ui.Web.Dto.Form                                  ( CreateUnlockLinkForm(..) )
import           Lib.Ui.Web.Route                                     ( linkAsText )

matchPartial :: ByteString -> MatchBody
matchPartial expected = MatchBody $ \_ body -> if B.isInfixOf expected $ show body
  then Nothing
  else Just $ "Expected to find somewhere in body: " <> decodeUtf8 expected <> ",\n" <> "Found: " <> decodeUtf8 body

withTestEnv :: ActionWith (AppEnv, Application) -> IO ()
withTestEnv = bracket setup clean
 where
  setup :: IO (AppEnv, Application)
  setup = do
    -- fix terminal encoding
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    env@Env.Env {..} <- mkAppEnv =<< setConfig =<< loadConfig Nothing
    runAppLogIO_ env $ Init.datafolder dataFolder
    Init.useRestrictedHttpManager
    void . async $ Server.persistenceApp env
    void . async $ testServer env
    pure (env, Server.application 6979 env)

  clean :: (AppEnv, Application) -> IO ()
  clean (Env.Env {..}, _app) = removeDirectoryRecursive dataFolder

  setConfig :: Config -> IO Config
  setConfig conf = do
    tmpDir   <- getTemporaryDirectory
    filePath <- addTrailingPathSeparator <$> createTempDirectory tmpDir "eselsohr_test"
    pure $ conf { Config.confDataFolder               = filePath
                , Config.confDepMode                  = Env.Test
                , Config.confPublicCollectionCreation = True
                }

  testServer :: AppEnv -> IO ()
  testServer = runSettings (warpSettings "127.0.0.1" 6980) . Server.application 6980

redirectLocation :: (MonadFail m) => SResponse -> m ByteString
redirectLocation SResponse {..} = do
  case find ((==) hLocation . fst) simpleHeaders of
    Nothing     -> fail "Missing location header"
    Just header -> pure $ snd header

accAsByteString :: ByteString -> ByteString
accAsByteString = B.drop 1 . B.dropWhile (/= '=')

accFromPath :: (MonadFail m) => ByteString -> m Accesstoken
accFromPath location = case toAccesstoken location of
  Left  err -> fail $ show err
  Right acc -> pure acc
  where toAccesstoken = parseUrlPiece . decodeUtf8 . B.drop 1 . B.dropWhile (/= '=')

accFromResponse :: (MonadFail m) => SResponse -> m Accesstoken
accFromResponse = accFromPath <=< redirectLocation

linkAsByteString :: Link -> ByteString
linkAsByteString = encodeUtf8 . linkAsText

postForm :: ByteString -> LByteString -> WaiSession st SResponse
postForm path = request methodPost path [(hContentType, "application/x-www-form-urlencoded")]

postCreateCollection :: WaiSession st SResponse
postCreateCollection = Test.post (linkAsByteString $ fieldLink Route.createCollection) ""

postCreateUnlockLink :: Accesstoken -> WaiSession st SResponse
postCreateUnlockLink = postForm unlockLinkRoute . unlockLinkForm
 where
  unlockLinkForm :: Accesstoken -> LByteString
  unlockLinkForm acc = urlEncodeAsForm $ CreateUnlockLinkForm acc (overviewRoute acc) Nothing Nothing

  overviewRoute :: Accesstoken -> Text
  overviewRoute = linkAsText . fieldLink Route.overviewPage . Just

  unlockLinkRoute :: ByteString
  unlockLinkRoute = linkAsByteString $ fieldLink Route.createUnlockLink

getOverviewPage :: Accesstoken -> WaiSession st ByteString
getOverviewPage acc = toStrict . simpleBody <$> Test.get (overviewRoute acc)
  where overviewRoute = linkAsByteString . fieldLink Route.overviewPage . Just
