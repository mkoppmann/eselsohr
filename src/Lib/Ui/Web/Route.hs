module Lib.Ui.Web.Route
    ( AppServer
    , ToApi
    , Api
    , HtmlPage
    , Redirection
    , CollectionSite (..)
    , ArticleListSite (..)
    , StaticSite (..)
    , ResourcesSite (..)
    , linkAsText
    ) where

import Lucid (Html)
import Servant.API
    ( Capture
    , Delete
    , FormUrlEncoded
    , Get
    , Link
    , Patch
    , Post
    , QueryParam
    , Raw
    , ReqBody
    , (:<|>)
    , (:>)
    )
import Servant.API.Generic
    ( GenericMode ((:-))
    , ToServantApi
    )
import Servant.HTML.Lucid (HTML)
import Servant.Links (linkURI)
import Servant.Server.Generic (AsServerT)

import Lib.Domain.Article (Article)
import Lib.Domain.Capability (Capability)
import Lib.Domain.Id (Id)
import Lib.Infra.Monad (App)
import Lib.Ui.Dto.Accesstoken (Accesstoken)
import Lib.Ui.Web.Dto.Form
    ( ChangeArticleStateForm
    , ChangeArticleTitleForm
    , CreateArticleForm
    , CreateSharedArticleListRefForm
    , CreateSharedArticleRefForm
    , CreateSharedOverviewRefForm
    , CreateUnlockLinkForm
    , DeleteItemForm
    )

type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site

type Api = Collection :<|> ArticleList :<|> Static :<|> Resources

type HtmlPage = Html ()

type Redirection = HtmlPage

data CollectionSite route = CollectionSite
    { createCollection
        :: route
            :- "collections"
                :> Post '[HTML] Redirection
    , overviewPage
        :: route
            :- "collections"
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , createUnlockLink
        :: route
            :- "collections"
                :> "unlock-links"
                :> ReqBody '[FormUrlEncoded] CreateUnlockLinkForm
                :> Post '[HTML] Redirection
    , deleteUnlockLink
        :: route
            :- "collections"
                :> "unlock-links"
                :> Capture "unlockLinkId" (Id Capability)
                :> ReqBody '[FormUrlEncoded] DeleteItemForm
                :> Delete '[HTML] Redirection
    , shareOverviewPage
        :: route
            :- "collections"
                :> "shared-ref"
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , createSharedOverviewRef
        :: route
            :- "collections"
                :> "shared-ref"
                :> ReqBody '[FormUrlEncoded] CreateSharedOverviewRefForm
                :> Post '[HTML] Redirection
    , deleteSharedOverviewRef
        :: route
            :- "collection"
                :> "shared-ref"
                :> Capture "sharedOverviewIdRef" (Id Capability)
                :> ReqBody '[FormUrlEncoded] DeleteItemForm
                :> Delete '[HTML] Redirection
    }
    deriving stock (Generic)

type Collection = ToApi CollectionSite

data ArticleListSite route = ArticleListSite
    { articleListPage
        :: route
            :- "articles"
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , createArticlePage
        :: route
            :- "articles"
                :> "new"
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , createArticle
        :: route
            :- "articles"
                :> ReqBody '[FormUrlEncoded] CreateArticleForm
                :> Post '[HTML] Redirection
    , articlePage
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , editArticlePage
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> "edit"
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , changeArticleTitle
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> "changeTitle"
                :> ReqBody '[FormUrlEncoded] ChangeArticleTitleForm
                :> Patch '[HTML] Redirection
    , markArticleAsRead
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> "markAsRead"
                :> ReqBody '[FormUrlEncoded] ChangeArticleStateForm
                :> Patch '[HTML] Redirection
    , markArticleAsUnread
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> "markAsUnread"
                :> ReqBody '[FormUrlEncoded] ChangeArticleStateForm
                :> Patch '[HTML] Redirection
    , deleteArticle
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> ReqBody '[FormUrlEncoded] DeleteItemForm
                :> Delete '[HTML] Redirection
    , shareArticleListPage
        :: route
            :- "articles"
                :> "shared-links"
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , createSharedArticleListRef
        :: route
            :- "articles"
                :> "shared-ref"
                :> ReqBody '[FormUrlEncoded] CreateSharedArticleListRefForm
                :> Post '[HTML] Redirection
    , deleteSharedArticleListRef
        :: route
            :- "articles"
                :> "shared-ref"
                :> Capture "sharedArticleListIdRef" (Id Capability)
                :> ReqBody '[FormUrlEncoded] DeleteItemForm
                :> Delete '[HTML] Redirection
    , shareArticlePage
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> "shared-links"
                :> QueryParam "acc" Accesstoken
                :> Get '[HTML] HtmlPage
    , createSharedArticleRef
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> "shared-ref"
                :> ReqBody '[FormUrlEncoded] CreateSharedArticleRefForm
                :> Post '[HTML] Redirection
    , deleteSharedArticleRef
        :: route
            :- "articles"
                :> Capture "articleId" (Id Article)
                :> "shared-ref"
                :> Capture "sharedArticleIdRef" (Id Capability)
                :> ReqBody '[FormUrlEncoded] DeleteItemForm
                :> Delete '[HTML] Redirection
    }
    deriving stock (Generic)

type ArticleList = ToApi ArticleListSite

data StaticSite route = StaticSite
    { startpage
        :: route
            :- Get '[HTML] HtmlPage
    , invalidToken
        :: route
            :- "invalid-token"
                :> Get '[HTML] HtmlPage
    }
    deriving stock (Generic)

type Static = ToApi StaticSite

newtype ResourcesSite route = ResourcesSite
    { resources
        :: route
            :- "static"
                :> Raw
    }
    deriving stock (Generic)

type Resources = ToApi ResourcesSite

linkAsText :: Link -> Text
linkAsText = (<>) "/" . show @Text . linkURI
