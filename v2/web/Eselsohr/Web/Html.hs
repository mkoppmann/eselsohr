{- |
Module      : Eselsohr.Web.Html
Description : Fixed pages of the web interface

'renderPage' renders each t'Page' as a complete HTML document with fixed text.
No page takes request data, so none can echo a bearer, a path, or submitted text.
'document' wraps a heading and body in the same document frame, for pages built from stored data.

'InvalidLinkPage' answers every request that matches no route or presents an unusable bearer, whatever the reason.
'DeletionNoticePage' is the one page that needs no bearer: a holder whose link was limited to an article lands there after deleting it.
The error pages answer malformed requests, oversized bodies, a busy collection, and server failures.
-}
module Eselsohr.Web.Html
    ( Page (..)
    , renderPage
    , document
    ) where

import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Lucid

data Page
    = InvalidLinkPage
    | DeletionNoticePage
    | BadRequestPage
    | BodyTooLargePage
    | BusyPage
    | ServerErrorPage
    deriving stock (Eq, Show, Enum, Bounded)

renderPage :: Page -> LazyByteString
renderPage =
    renderBS . \case
        InvalidLinkPage ->
            layout "Link not valid" "This link does not lead to a collection. It may be mistyped, expired, or revoked."
        DeletionNoticePage ->
            layout "Article deleted" "The article was deleted. The link you used was limited to this article and no longer works."
        BadRequestPage ->
            layout "Bad request" "The server could not understand the request."
        BodyTooLargePage ->
            layout "Request too large" "The submitted form is larger than the server accepts."
        BusyPage ->
            layout "Busy" "The collection is busy. Please try again in a moment."
        ServerErrorPage ->
            layout "Server error" "The request could not be completed. Please try again later."

layout :: Text -> Text -> Html ()
layout heading = document heading . p_ . toHtml

-- | A complete document whose title and first heading are the given heading.
document :: Text -> Html () -> Html ()
document heading content = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ (toHtml heading <> " - Eselsohr")
    body_ . main_ $ do
        h1_ (toHtml heading)
        content
