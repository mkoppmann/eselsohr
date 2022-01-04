module Net.IPv6.Helper
  ( public
  , private
  ) where

import           Net.IPv6                                             ( IPv6
                                                                      , contains
                                                                      , ipv6
                                                                      , localhost
                                                                      , range
                                                                      )

public :: IPv6 -> Bool
public = not . private

private :: IPv6 -> Bool
private ip = localhost == ip || contains uniqueLocalUnicast ip || contains linkLocalUnicastAddress ip
 where
  uniqueLocalUnicast      = range (ipv6 0xFC00 0x0 0x0 0x0 0x0 0x0 0x0 0x0) 7
  linkLocalUnicastAddress = range (ipv6 0xFE80 0x0 0x0 0x0 0x0 0x0 0x0 0x0) 10
