module Lib.Core.Effect.Scraper
  ( MonadScraper (..),
  )
where

import Lib.Core.Domain (Uri)
import UnliftIO (MonadUnliftIO)

class (MonadUnliftIO m) => MonadScraper m where
  scrapWebsite :: Uri -> m Text
