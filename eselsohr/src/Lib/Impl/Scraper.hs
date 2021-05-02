module Lib.Impl.Scraper
  ( scrapWebsite
  ) where

import           Lib.Core.Domain                ( Uri )
import qualified Lib.Core.Domain.Uri           as Uri
import qualified Text.HTML.Scalpel             as SC

type WithScraper m = MonadIO m

scrapWebsite :: WithScraper m => Uri -> m Text
scrapWebsite uri = fromMaybe "Empty Title" <$> scrapWebsiteDirect uri

scrapWebsiteDirect :: WithScraper m => Uri -> m (Maybe Text)
scrapWebsiteDirect uri =
  liftIO $ SC.scrapeURL (toString $ Uri.render uri) titleScraper

titleScraper :: SC.Scraper Text Text
titleScraper = SC.text "h1"
