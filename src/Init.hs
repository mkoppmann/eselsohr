module Init
    ( datafolder
    , useRestrictedHttpManager
    ) where

import Net.IPv4 qualified as IPv4
import Net.IPv6 qualified as IPv6
import Net.IPv6.Helper qualified as IPv6
import Network.HTTP.Client.Restricted qualified as Client

import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
    ( newTlsManagerWith
    , setGlobalManager
    )
import Network.Socket
    ( AddrInfo (addrAddress)
    , SockAddr (..)
    , hostAddress6ToTuple
    , hostAddressToTuple
    )
import UnliftIO.Directory (createDirectoryIfMissing)

datafolder :: (MonadIO m) => FilePath -> m ()
datafolder = liftIO . createDirectoryIfMissing True

useRestrictedHttpManager :: (MonadIO m) => m ()
useRestrictedHttpManager = liftIO $ setGlobalManager =<< restrictedManager
  where
    -- We are using a restricted version of the HTTP client here, that protects
    -- us against SSRF and DNS rebinding attacks.
    restrictedManager :: (MonadIO m) => m Manager
    restrictedManager = do
        let connRestricted = Client.connectionRestricted ("This IP address is not allowed: " <>)
            restriction = Client.addressRestriction $
                \addr -> if isAllowed $ addrAddress addr then Nothing else Just $ connRestricted addr
        (settings, _) <- liftIO $ Client.mkRestrictedManagerSettings restriction Nothing Nothing
        liftIO $ newTlsManagerWith settings

    isAllowed :: SockAddr -> Bool
    isAllowed (SockAddrInet _ ipv4) = IPv4.public . IPv4.fromTupleOctets $ hostAddressToTuple ipv4
    isAllowed (SockAddrInet6 _ _ ipv6 _) = IPv6.public . IPv6.fromTupleWord16s $ hostAddress6ToTuple ipv6
    isAllowed (SockAddrUnix _) = False
