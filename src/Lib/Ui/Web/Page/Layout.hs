module Lib.Ui.Web.Page.Layout
    ( render
    , renderM
    ) where

import Lucid

import Lib.Ui.Web.Route (HtmlPage)

render :: Html () -> Html ()
render page = doctypehtml_ $ do
    head_ $ do
        title_ "Eselsohr"
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        link_ [rel_ "stylesheet", type_ "text/css", href_ "static/style.css"]
    body_ $ do
        header
        main_ page
        footer

renderM :: (Monad m) => Html () -> m HtmlPage
renderM = pure . render

header :: Html ()
header = header_ ""

footer :: Html ()
footer = footer_ $ p_ "Eselsohr 📄 (α)"
