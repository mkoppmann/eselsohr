module Scraper where

import qualified Text.HTML.Scalpel as SC

fetchTitleWithDefault :: LText -> IO LText
fetchTitleWithDefault url = do
  mTitle <- fetchTitle url
  return $ fromMaybe "Empty Title" mTitle

fetchTitle :: LText -> IO (Maybe LText)
fetchTitle url = SC.scrapeURL (toString url) title
  where
    title :: SC.Scraper LText LText
    title = SC.text "h1"
