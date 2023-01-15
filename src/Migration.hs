module Migration
    ( migrate
    ) where

import UnliftIO.Directory (listDirectory)

import Lib.Infra.Persistence.Model.Collection qualified as Collection

import Lib.Infra.Persistence.File
    ( decodeFile
    , encodeFile
    )

migrate :: (MonadIO m) => FilePath -> m ()
migrate fp = do
    print @Text "Starting migration."
    collectionPaths <- (fp <>) <<$>> listDirectory fp
    traverse_ migrateCollection collectionPaths
    print @Text "Migration done."

migrateCollection :: (MonadIO m) => FilePath -> m ()
migrateCollection fp = encodeFile fp . Collection.migrate =<< decodeFile fp
