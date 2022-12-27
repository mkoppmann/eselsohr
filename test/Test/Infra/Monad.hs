module Test.Infra.Monad
    ( TestApp (..)
    , TestAppEnv
    , runAppAsIO
    ) where

import Control.Monad.Except (MonadError (..))

import qualified Lib.Infra.Adapter.Random as RandomImpl
import qualified Lib.Infra.Adapter.Time as TimeImpl
import qualified Test.Infra.Repo.ArticleList as ArtRepo
import qualified Test.Infra.Repo.CapabilityList as CapRepo
import qualified Test.Infra.Repo.Collection as ColRepo

import Control.Exception
    ( catch
    , throwIO
    , try
    )
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
import Relude.Extra (firstF)
import Test.App.Env (TestEnv)

type TestAppEnv = TestEnv TestApp

newtype TestApp a = TestApp {unApp :: ReaderT TestAppEnv IO a}
    deriving
        (Functor, Applicative, Monad, MonadIO, MonadReader TestAppEnv)
        via ReaderT TestAppEnv IO

instance MonadError AppError TestApp where
    throwError :: AppError -> TestApp a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError :: TestApp a -> (AppError -> TestApp a) -> TestApp a
    catchError action handler = TestApp $ ReaderT $ \env ->
        let ioAction = runApp env action in ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

instance MonadRandom TestApp where
    getRandomId = RandomImpl.getRandomId

instance MonadScraper TestApp where
    scrapWebsite = pure . toText

instance MonadTime TestApp where
    getCurrentTime = TimeImpl.getCurrentTime

instance ArticleListRepo TestApp where
    loadAll = ArtRepo.testLoadAll
    nextId = RandomImpl.getRandomId
    saveAll = ArtRepo.testSaveAll

instance CapabilityListRepo TestApp where
    loadAll = CapRepo.testLoadAll
    nextId = RandomImpl.getRandomId
    saveAll = CapRepo.testSaveAll

instance CollectionRepo TestApp where
    exists = ColRepo.testExists
    createCollection = ColRepo.testCreateCollection

runAppAsIO :: TestAppEnv -> TestApp a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env

runApp :: TestAppEnv -> TestApp a -> IO a
runApp env = usingReaderT env . unApp
