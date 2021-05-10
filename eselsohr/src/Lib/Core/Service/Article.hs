module Lib.Core.Service.Article
  ( getArticles
  , getArticle
  , createArticle
  , changeArticleTitle
  , changeArticleState
  , deleteArticle
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as Seq
import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( Article(..)
                                                , ArticleState(..)
                                                , AuthAction
                                                , Entity(..)
                                                , Uri
                                                )
import           Lib.Core.Effect                ( ContextState(..)
                                                , MonadScraper(..)
                                                , MonadTime(..)
                                                , WriteState(..)
                                                )
import qualified Lib.Core.Effect.Repository    as R
import           Lib.Core.Service.Util          ( getResId
                                                , notFound
                                                )

getArticles
  :: (WithError m) => ContextState -> AuthAction -> m (Seq (Entity Article))
getArticles ctxState authAction = do
  let sRes = csResource ctxState
  arts <- R.getAllArt sRes authAction
  pure
    . fmap (uncurry Entity)
    . Seq.sortBy sortAfterDate
    . Seq.fromList
    $ Map.toList arts
  where sortAfterDate (_, art1) (_, art2) = compare art1 art2

getArticle :: (WithError m) => ContextState -> AuthAction -> m (Entity Article)
getArticle ctxState authAction = do
  let sRes = csResource ctxState
  maybe notFound pure =<< R.lookupArt sRes authAction

createArticle
  :: (WriteState m, MonadScraper m, MonadTime m, WithError m)
  => ContextState
  -> Uri
  -> AuthAction
  -> m ()
createArticle ctxState uri authAction = do
  let resId = getResId ctxState
  R.commit resId . one =<< R.insertArt authAction =<< createArticle'
 where
  createArticle' :: (MonadScraper m, MonadTime m) => m Article
  createArticle' = do
    aTitle   <- scrapWebsite uri
    aCreated <- getCurrentTime
    let aState = Unread
    pure $ Article aTitle uri aState aCreated

changeArticleTitle
  :: (WriteState m, WithError m) => ContextState -> Text -> AuthAction -> m ()
changeArticleTitle ctxState aTitle authAction = do
  let resId = getResId ctxState
  R.commit resId . one =<< R.updateArtTitle authAction aTitle

changeArticleState
  :: (WriteState m, WithError m)
  => ContextState
  -> ArticleState
  -> AuthAction
  -> m ()
changeArticleState ctxState artState authAction = do
  let resId = getResId ctxState
  R.commit resId . one =<< R.updateArtState authAction artState

deleteArticle
  :: (WriteState m, WithError m) => ContextState -> AuthAction -> m ()
deleteArticle ctxState authAction = do
  let resId = getResId ctxState
  R.commit resId . one =<< R.deleteArt authAction
