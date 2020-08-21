module Lib.Effect.Scraper
  ( MonadScraper (..),
  )
where

import Lib.App (App (..))
import Lib.Core.Uri (Uri (Uri))
import qualified Text.HTML.Scalpel as SC
import Text.URI (render)

class Monad m => MonadScraper m where
  scrapWebsite :: Uri -> m LText

instance MonadScraper App where
  scrapWebsite = scrapWebsiteWithDefaults

type WithScraper m = MonadIO m

scrapWebsiteWithDefaults :: WithScraper m => Uri -> m LText
scrapWebsiteWithDefaults uri =
  fromMaybe "Empty Title" <$> scrapWebsiteDirect uri

scrapWebsiteDirect :: WithScraper m => Uri -> m (Maybe LText)
scrapWebsiteDirect uri =
  let sUri = toString . render $ coerce uri
   in liftIO $ SC.scrapeURL sUri titleScraper

titleScraper :: SC.Scraper LText LText
titleScraper = SC.text "h1"
