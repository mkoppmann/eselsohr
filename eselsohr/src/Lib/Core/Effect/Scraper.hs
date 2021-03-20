module Lib.Core.Effect.Scraper
  ( MonadScraper (..),
  )
where

import Lib.Core.Domain (Uri)

class (Monad m) => MonadScraper m where
  scrapWebsite :: Uri -> m Text
