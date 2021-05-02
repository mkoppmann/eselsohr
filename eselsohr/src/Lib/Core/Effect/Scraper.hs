module Lib.Core.Effect.Scraper
  ( MonadScraper(..)
  ) where

import           Lib.App                        ( App )
import           Lib.Core.Domain                ( Uri )
import qualified Lib.Impl.Scraper              as Impl

class (Monad m) => MonadScraper m where
  scrapWebsite :: Uri -> m Text

instance MonadScraper App where
  scrapWebsite = Impl.scrapWebsite
