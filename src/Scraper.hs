module Scraper where

import qualified Text.HTML.Scalpel as SC

scrapWebsiteWithDefaults :: LText -> IO LText
scrapWebsiteWithDefaults url = do
  mScrap <- scrapWebsite url
  return $ fromMaybe "Empty Title" mScrap

scrapWebsite :: LText -> IO (Maybe LText)
scrapWebsite url = SC.scrapeURL (toString url) titleScraper

titleScraper :: SC.Scraper LText LText
titleScraper = SC.text "h1"
