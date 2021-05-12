module Lib.Web.Controller.Article
  ( article
  ) where

import           Lib.App                        ( WithError )
import           Lib.Core.Domain                ( Article
                                                , ArticlePerms(..)
                                                , ArticlesPerms(..)
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
                                                , getCapId
                                                , getContextState
                                                , getObjRef
                                                , missingParameter
                                                , notAuthorized
                                                , redirect
                                                )
import qualified Lib.Web.Route                 as Route
import           Lib.Web.Types                  ( AppServer
                                                , DeleteItemForm(..)
                                                , PatchArticleForm(..)
                                                , PostCreateArticleForm(..)
                                                , PostCreateSharedArticleRefForm(..)
                                                , PostCreateSharedArticlesRefForm(..)
                                                , Redirection
                                                )

article :: Route.ArticlesSite AppServer
article = Route.ArticlesSite
  { Route.createArticle           = createArticle
  , Route.createSharedArticlesRef = createSharedArticlesRef
  , Route.createSharedArticleRef  = createSharedArticleRef
  , Route.patchArticle            = patchArticle
  , Route.deleteArticle           = deleteArticle
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

createSharedArticlesRef
  :: (RWState m, MonadRandom m, MonadTime m, WithError m)
  => PostCreateSharedArticlesRefForm
  -> m Redirection
createSharedArticlesRef PostCreateSharedArticlesRefForm {..} = do
  ctxState <- getContextState acc
  let ogCapId = getCapId acc
      objRef  = getObjRef ctxState

  sharedRefId <- getRandomId
  let mAuthAct = Cap.createSharedRef objRef sharedRefId
  sharedRef <- maybe notAuthorized pure
    $ Cap.createSharedArticlesRef ogCapId objRef perms

  authAction
    (Service.createSharedRef ctxState petname expirationDate sharedRef)
    mAuthAct
  redirect $ render goto
 where
  perms :: ArticlesPerms
  perms = ArticlesPerms (Cap.maybePerm viewArticles)
                        (Cap.maybePerm createArticles)
                        (Cap.maybePerm changeTitle)
                        (Cap.maybePerm changeState)
                        (Cap.maybePerm delete)
                        (Cap.maybePerm shareLinks)

createSharedArticleRef
  :: (RWState m, MonadRandom m, MonadTime m, WithError m)
  => Id Article
  -> PostCreateSharedArticleRefForm
  -> m Redirection
createSharedArticleRef artId PostCreateSharedArticleRefForm {..} = do
  ctxState <- getContextState acc
  let ogCapId = getCapId acc
      objRef  = getObjRef ctxState

  sharedRefId <- getRandomId
  let mAuthAct = Cap.createSharedRef objRef sharedRefId
  sharedRef <- maybe notAuthorized pure
    $ Cap.createSharedArticleRef ogCapId objRef perms artId

  authAction
    (Service.createSharedRef ctxState petname expirationDate sharedRef)
    mAuthAct
  redirect $ render goto
 where
  perms :: ArticlePerms
  perms = ArticlePerms (Cap.maybePerm viewArticle)
                       (Cap.maybePerm changeTitle)
                       (Cap.maybePerm changeState)
                       (Cap.maybePerm delete)
                       (Cap.maybePerm shareLinks)

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
