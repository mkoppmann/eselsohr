module Lib.App.Monad
  ( App(..)
  , AppEnv
  , runAppAsIO
  ) where

import           Control.Monad.Except           ( MonadError(..) )
import           Lib.App.Env                    ( Env )
import           Lib.App.Error                  ( AppError
                                                , AppException(..)
                                                )
import           Lib.Core.Effect.Random         ( MonadRandom(..) )
import           Lib.Core.Effect.Scraper        ( MonadScraper(..) )
import           Lib.Core.Effect.Time           ( MonadTime(..) )
import qualified Lib.Impl.Random               as RandomImpl
import qualified Lib.Impl.Scraper              as ScraperImpl
import qualified Lib.Impl.Time                 as TimeImpl
import           Relude.Extra.Bifunctor         ( firstF )
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Exception             ( catch
                                                , throwIO
                                                , try
                                                )

-- | 'Env' data type parameterized by 'App' monad
type AppEnv = Env App

-- | Main application monad.
newtype App a = App {unApp :: ReaderT AppEnv IO a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO)
    via ReaderT AppEnv IO

instance MonadError AppError App where
  throwError :: AppError -> App a
  throwError = liftIO . throwIO . AppException
  {-# INLINE throwError #-}

  catchError :: App a -> (AppError -> App a) -> App a
  catchError action handler = App $ ReaderT $ \env ->
    let ioAction = runApp env action
    in  ioAction `catch` \(AppException e) -> runApp env $ handler e
  {-# INLINE catchError #-}

{- | Helper for running route handlers in IO. Catches exception of type
 'AppException' and unwraps 'AppError' from it.
 Do not use this function to run the application. Use runners with logging from
 "Lib.Effects.Log" module to also log the error.
-}
runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env

{- | Helper for running 'App'.
 Do not use this function to run the application. Use runners with logging from
 "Lib.Effects.Log" module to also log the error.
-}
runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp

-- * Monad effect instances

instance MonadRandom App where
  getRandomId = RandomImpl.getRandomId

instance MonadScraper App where
  scrapWebsite = ScraperImpl.scrapWebsite

instance MonadTime App where
  getCurrentTime = TimeImpl.getCurrentTime
