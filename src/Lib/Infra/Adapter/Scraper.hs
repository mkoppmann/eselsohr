module Lib.Infra.Adapter.Scraper
  ( scrapWebsite
  ) where

import qualified Text.HTML.Scalpel                                   as SC

import           Lib.Domain.Uri                                       ( Uri )

type WithScraper m = MonadIO m

scrapWebsite :: WithScraper m => Uri -> m Text
scrapWebsite uri = fromMaybe "Empty Title" <$> scrapWebsiteDirect uri

scrapWebsiteDirect :: WithScraper m => Uri -> m (Maybe Text)
scrapWebsiteDirect uri = liftIO $ SC.scrapeURL (toString $ toText uri) titleScraper

titleScraper :: SC.Scraper Text Text
titleScraper = SC.text "h1"
