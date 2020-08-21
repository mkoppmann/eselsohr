module Lib.Web.View.App
  ( render,
  )
where

import Lucid
import Lucid.Servant (linkAbsHref_)
import Servant (Link)

render :: Link -> Html () -> Html ()
render stylesheetRoute page = doctypehtml_ $ do
  head_ $ do
    title_ "Eselsohr"
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    link_ [rel_ "stylesheet", linkAbsHref_ stylesheetRoute]
  body_ $ do
    header
    main_ page
    footer

header :: Html ()
header = do
  header_ $ do
    ""

footer :: Html ()
footer = do
  footer_ $ do
    p_ "Elegance through simplicity"
