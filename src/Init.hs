module Init
    ( datafolder
    ) where

import UnliftIO.Directory (createDirectoryIfMissing)

datafolder :: (MonadIO m) => FilePath -> m ()
datafolder = liftIO . createDirectoryIfMissing True
