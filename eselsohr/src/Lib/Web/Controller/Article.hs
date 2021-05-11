module Lib.Web.Controller.Article
  ( article
  ) where

import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( Article
                                                , Id
                                                , render
                                                )
import qualified Lib.Core.Domain.Capability    as Cap
import           Lib.Core.Effect                ( MonadRandom(..)
                                                , MonadScraper
                                                , MonadTime
                                                , RWState
                                                )
import qualified Lib.Core.Service              as Service
import           Lib.Web.Controller.Util        ( authAction
                                                , getContextState
                                                , getObjRef
                                                , missingParameter
                                                , redirect
                                                )
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( AppServer
                                                , DeleteItemForm(..)
                                                , PatchArticleForm(..)
                                                , PostCreateArticleForm(..)
                                                , Redirection
                                                )

article :: Route.ArticlesSite AppServer
article = Route.ArticlesSite { Route.createArticle = createArticle
                             , Route.patchArticle  = patchArticle
                             , Route.deleteArticle = deleteArticle
                             }

createArticle
  :: (RWState m, MonadRandom m, MonadScraper m, MonadTime m, WithError m)
  => PostCreateArticleForm
  -> m Redirection
createArticle PostCreateArticleForm {..} = do
  ctxState <- getContextState acc
  artId    <- getRandomId
  let mAuthAct = Cap.createArticle (getObjRef ctxState) artId
  authAction (Service.createArticle ctxState articleUri) mAuthAct
  redirect $ render goto

patchArticle
  :: (RWState m, MonadTime m, WithError m)
  => Id Article
  -> PatchArticleForm
  -> m Redirection
patchArticle artId PatchArticleForm {..} = do
  ctxState <- getContextState acc
  let objRef = getObjRef ctxState

  whenJust articleTitle (changeArticleTitle ctxState objRef)
  whenJust articleState (changeArticleState ctxState objRef)
  when (isNothing articleTitle && isNothing articleState)
    $ missingParameter "articleTitle or articleState"

  redirect $ render goto
 where
  changeArticleTitle ctxState objRef artTitle = do
    let mAuthAct = Cap.changeArticleTitle objRef artId
    authAction (Service.changeArticleTitle ctxState artTitle) mAuthAct

  changeArticleState ctxState objRef artState = do
    let mAuthAct = Cap.changeArticleState objRef artId
    authAction (Service.changeArticleState ctxState artState) mAuthAct

deleteArticle
  :: (RWState m, MonadTime m, WithError m)
  => Id Article
  -> DeleteItemForm
  -> m Redirection
deleteArticle artId DeleteItemForm {..} = do
  ctxState <- getContextState acc
  let mAuthAct = Cap.deleteArticle (getObjRef ctxState) artId
  authAction (Service.deleteArticle ctxState) mAuthAct
  redirect $ render goto
