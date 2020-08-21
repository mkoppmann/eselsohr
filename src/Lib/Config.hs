module Lib.Config
  ( Config (..),
    loadConfig,
  )
where

import Colog (Severity (Debug))
import Network.Wai.Handler.Warp (Port)

data Config = Config
  { confDataFolder :: !FilePath,
    confLogSeverity :: !Severity,
    confServerPort :: !Port
  }

loadConfig :: Config
loadConfig = Config "data/" Debug 6979
