module Lib.App.Port
  ( MonadRandom(..)
  , MonadScraper(..)
  , MonadTime(..)
  ) where

import           Data.Time.Clock                ( UTCTime )

import           Lib.Domain.Id                  ( Id )
import           Lib.Domain.Uri                 ( Uri )

class (Monad m) => MonadRandom m where
  getRandomId :: m (Id a)

class (Monad m) => MonadScraper m where
  scrapWebsite :: Uri -> m Text

class (Monad m) => MonadTime m where
  getCurrentTime :: m UTCTime
