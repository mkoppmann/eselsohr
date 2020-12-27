module Lib.Persistence.File
  ( WithFile,
  )
where

import Lib.App (DataPath, Has)

type WithFile env m = (MonadReader env m, Has DataPath env, MonadIO m)
