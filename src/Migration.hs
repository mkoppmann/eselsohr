{- | Entrypoint for starting the migration process to newer data storage
version.
-}

module Migration
  ( migrate
  ) where


import           UnliftIO.Directory                                   ( listDirectory )

import qualified Lib.Infra.Persistence.Model.Collection              as Collection

import           Lib.Infra.Persistence.File                           ( decodeFile
                                                                      , encodeFile
                                                                      )

-- | Start migration process.
migrate :: (MonadIO m) => FilePath -> m ()
migrate fp = do
  print @Text "Starting migration."
  collectionPaths <- (fp <>) <<$>> listDirectory fp
  traverse_ migrateCollection collectionPaths
  print @Text "Migration done."

migrateCollection :: (MonadIO m) => FilePath -> m ()
migrateCollection fp = encodeFile fp . Collection.migrate =<< decodeFile fp
