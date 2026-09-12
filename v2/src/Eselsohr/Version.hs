{- |
Module      : Eselsohr.Version
Description : Package version

'version' is the printable version from the Cabal package metadata.
The executable uses it for @--version@ output.
-}
module Eselsohr.Version (version) where

import Data.Version (showVersion)
import Paths_eselsohr_v2 qualified as Package

version :: String
version = showVersion Package.version
