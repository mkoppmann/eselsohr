module Lib.Init
  ( datafolder,
    initialCap,
    useRestrictedHttpManager,
  )
where

import qualified Data.Binary as Bin
import qualified Data.Map.Strict as Map
import qualified Data.UUID.V4 as UUID
import Lib.Core.Domain.Capability (Action (..), Capability (..), CommandAction (..), FrontendAction (..), PostAction (..), QueryAction (..), ResourceOverviewActions (..))
import Lib.Core.Domain.Id (Id)
import qualified Lib.Core.Domain.Id as Id
import Lib.Core.Domain.Resource (CapabilityCollection (..), Resource (..))
import qualified Lib.Core.Effect.Repository as R
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Net.IPv6.Helper as IPv6
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client.Restricted as Client
import Network.HTTP.Client.TLS (newTlsManagerWith, setGlobalManager)
import Network.Socket (AddrInfo (addrAddress), SockAddr (..), hostAddress6ToTuple, hostAddressToTuple)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)

datafolder :: (MonadIO m) => FilePath -> m ()
datafolder = liftIO . createDirectoryIfMissing True

initialCap :: (MonadIO m) => FilePath -> m ()
initialCap fp =
  sysFileExists >>= \fExists -> if fExists then prepareAccess else initFile
  where
    sysFileExists :: (MonadIO m) => m Bool
    sysFileExists = liftIO $ doesFileExist sysFilePath

    initFile :: (MonadIO m) => m ()
    initFile = liftIO $ Bin.encodeFile sysFilePath =<< createInitialCap

    prepareAccess :: (MonadIO m) => m ()
    prepareAccess = flip when initFile =<< lookupInitialCap

    lookupInitialCap :: (MonadIO m) => m Bool
    lookupInitialCap = do
      resource <- liftIO $ Bin.decodeFile sysFilePath
      case resource of
        ArticleResource _ -> pure False -- throw an exception here?
        CapOnlyResource capCol -> pure . Map.null $ capCollection capCol

    createInitialCap :: (MonadIO m) => m Resource
    createInitialCap = do
      let command = Command $ Post CreateResource
      let query =
            Query
              . ResourceOverview
              $ ResourceOverviewActions
                Id.mkNilId
                Id.mkNilId
                Nothing
                Nothing
      let redirectTo = "Change Me"
      (cId, cMap) <- storeInMap Nothing command Map.empty
      (qId, cqMap) <- storeInMap Nothing query cMap
      let frontend = Frontend $ FrontendAction cId qId redirectTo
      (fId, actMap) <- storeInMap Nothing frontend cqMap

      let cap = Capability Nothing Nothing fId
      (_, capMap) <- storeInMap (Just R.initialCapId) cap Map.empty
      pure . CapOnlyResource $ CapabilityCollection capMap actMap

    storeInMap ::
      (MonadIO m) =>
      Maybe (Id a) ->
      a ->
      Map (Id a) a ->
      m (Id a, Map (Id a) a)
    storeInMap mKey val aMap = case mKey of
      Just key -> pure (key, Map.insert key val aMap)
      Nothing -> do
        aId <- Id.Id <$> liftIO UUID.nextRandom
        pure (aId, Map.insert aId val aMap)

    sysFilePath :: FilePath
    sysFilePath =
      let fId = toString $ Id.toText R.systemColId
       in fp <> fId <> ".bin"

useRestrictedHttpManager :: (MonadIO m) => m ()
useRestrictedHttpManager = liftIO $ setGlobalManager =<< restrictedManager
  where
    -- We are using a restricted version of the HTTP client here, that protects
    -- us against SSRF and DNS rebinding attacks.
    restrictedManager :: (MonadIO m) => m Manager
    restrictedManager = do
      let connRestricted =
            Client.connectionRestricted
              ("This IP address is not allowed: " <>)
      let restriction = Client.addressRestriction $ \addr ->
            if isAllowed $ addrAddress addr
              then Nothing
              else Just $ connRestricted addr
      (settings, _) <-
        liftIO $ Client.mkRestrictedManagerSettings restriction Nothing Nothing
      liftIO $ newTlsManagerWith settings

isAllowed :: SockAddr -> Bool
isAllowed (SockAddrInet _ ipv4) =
  IPv4.public . IPv4.fromTupleOctets $ hostAddressToTuple ipv4
isAllowed (SockAddrInet6 _ _ ipv6 _) =
  IPv6.public . IPv6.fromTupleWord16s $ hostAddress6ToTuple ipv6
isAllowed (SockAddrUnix _) = False
