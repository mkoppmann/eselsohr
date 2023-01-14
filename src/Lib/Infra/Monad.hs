module Lib.Infra.Monad
    ( App (..)
    , AppEnv
    , runAppAsIO
    ) where

import Control.Monad.Except (MonadError (..))
import Relude.Extra (firstF)
import UnliftIO
    ( MonadUnliftIO
    , catch
    , throwIO
    , try
    )

import qualified Lib.Infra.Adapter.Random as RandomImpl
import qualified Lib.Infra.Adapter.Scraper as ScraperImpl
import qualified Lib.Infra.Adapter.Time as TimeImpl
import qualified Lib.Infra.Repo.ArticleList as ArtRepo
import qualified Lib.Infra.Repo.CapabilityList as CapRepo
import qualified Lib.Infra.Repo.Collection as ColRepo

import Lib.App.Env (Env)
import Lib.App.Port
    ( MonadRandom (..)
    , MonadScraper (..)
    , MonadTime (..)
    )
import Lib.Domain.Repo.ArticleList (ArticleListRepo (..))
import Lib.Domain.Repo.CapabilityList (CapabilityListRepo (..))
import Lib.Domain.Repo.Collection (CollectionRepo (..))
import Lib.Infra.Error
    ( AppError
    , AppException (..)
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
        let ioAction = runApp env action in ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

instance MonadRandom App where
    getRandomId = RandomImpl.getRandomId

instance MonadScraper App where
    scrapWebsite = ScraperImpl.scrapWebsite

instance MonadTime App where
    getCurrentTime = TimeImpl.getCurrentTime

instance ArticleListRepo App where
    loadAll = ArtRepo.loadAll
    nextId = ArtRepo.nextId
    saveAll = ArtRepo.saveAll

instance CapabilityListRepo App where
    loadAll = CapRepo.loadAll
    nextId = CapRepo.nextId
    saveAll = CapRepo.saveAll

instance CollectionRepo App where
    exists = ColRepo.exists
    createCollection = ColRepo.createCollection

{- | Helper for running route handlers in IO. Catches exception of type
 'AppException' and unwraps 'AppError' from it.
 Do not use this function to run the application. Use runners with logging from
 "Lib.Infra.Log" module to also log the error.
-}
runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unwrap . try . runApp env
  where
    unwrap (AppException err) = err

{- | Helper for running 'App'.
 Do not use this function to run the application. Use runners with logging from
 "Lib.Infra.Log" module to also log the error.
-}
runApp :: AppEnv -> App a -> IO a
runApp env app = usingReaderT env app.unApp
