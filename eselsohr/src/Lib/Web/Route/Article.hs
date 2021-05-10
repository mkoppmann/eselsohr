module Lib.Web.Route.Article
  ( Articles
  , ArticlesSite(..)
  ) where

import           Lib.Core.Domain                ( Article
                                                , Id
                                                )
import           Lib.Web.Types                  ( DeleteItemForm
                                                , PatchArticleForm
                                                , PostCreateArticleForm
                                                , Redirection
                                                , ToApi
                                                )
import           Servant                        ( (:>)
                                                , Capture
                                                , Delete
                                                , FormUrlEncoded
                                                , Patch
                                                , Post
                                                , ReqBody
                                                )
import           Servant.API.Generic            ( GenericMode((:-)) )
import           Servant.HTML.Lucid             ( HTML )

data ArticlesSite route = ArticlesSite
  { createArticle ::
      route
        :- "api"
        :> "articles"
        :> ReqBody '[FormUrlEncoded] PostCreateArticleForm
        :> Post '[HTML] Redirection
  , patchArticle ::
      route
        :- "api"
        :> "articles"
        :> Capture "articleId" (Id Article)
        :> ReqBody '[FormUrlEncoded] PatchArticleForm
        :> Patch '[HTML] Redirection
  , deleteArticle ::
      route
        :- "api"
        :> "articles"
        :> Capture "articleId" (Id Article)
        :> ReqBody '[FormUrlEncoded] DeleteItemForm
        :> Delete '[HTML] Redirection
  }
  deriving stock (Generic)

type Articles = ToApi ArticlesSite
