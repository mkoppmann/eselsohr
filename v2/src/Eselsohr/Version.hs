module Eselsohr.Version (version) where

import Data.Version (showVersion)
import Paths_eselsohr_v2 qualified as Package

version :: String
version = showVersion Package.version
