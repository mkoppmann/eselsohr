module View where

import qualified Clay as C
import qualified Clay.Flexbox as CF
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.UUID (UUID, toText)
import Model
import Routes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderApp :: Html -> LText
renderApp page = renderHtml $
  docTypeHtml $ do
    H.head $ do
      H.title "Eselsohr"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      link ! rel "stylesheet" ! href (lazyTextValue cssRoute)
    body $ do
      appHeader
      main $ do
        page
      appFooter

startpage :: Html
startpage = do
  h1 "Welcome to Eselsohr"
  p "Eselsohr is a service focussed on simplicity. Store web pages, videos, and more and consume them later. Start your collection by clicking on the button :)"
  newCollectionForm

collectionMappingList :: [CollectionMapping] -> Html
collectionMappingList collmaps = do
  h2 "Add a new collection"
  newCollectionForm
  h1 "The collection mappings list"
  ul $ mapM_ renderCollMaps collmaps
  where
    renderCollMaps = li . toHtml . prettyCollMap
    prettyCollMap (CollectionMapping collId acc) = do
      let lCollId = show $ getSqliteUUID $ getCollectionId collId :: LText
      p $ a ! href (routeWithAccesstokenToAttribute articlesRoute acc) $ toHtml lCollId
      deleteCollectionForm acc

articleList :: Accesstoken -> [Article] -> Html
articleList acc articles = do
  h2 "Add a new article"
  newArticleForm acc
  h1 "The mighty article list"
  H.div $ mapM_ renderArticle articles
  where
    renderArticle = (article ! class_ "item") . toHtml . articleItem acc

{- prettyArticle (Article aId aTitle _ created) = do
  p $ do
    toHtml $ prettyDate created <> "| "
    a ! href (routeWithAccesstokenAndIdToAttribute articleWithIdRoute acc aId) $ toHtml aTitle
  editArticleForm acc aId
  deleteArticleForm acc aId -}

articleItem :: Accesstoken -> Article -> Html
articleItem acc (Article aId aTitle _ aCreated) = do
  H.div ! class_ "item-header" $ do
    H.span ! class_ "item-title" $ do
      a ! href (routeWithAccesstokenAndIdToAttribute articleWithIdRoute acc aId) $ toHtml aTitle
  H.div ! class_ "item-meta" $ do
    H.ul ! class_ "item-meta-info" $ do
      li $ H.time ! A.datetime (lazyTextValue $ prettyDate aCreated) $ toHtml $ prettyDate aCreated
    H.ul ! class_ "item-meta-icons" $ do
      li $ editArticleForm acc aId
      li "|"
      li $ deleteArticleForm acc aId

articleDetails :: Accesstoken -> Article -> Html
articleDetails acc (Article aId aTitle aHref aCreated) = do
  h1 $ toHtml aTitle
  p $ toHtml $ "Created: " <> prettyDate aCreated
  p $ a ! href (lazyTextValue aHref) $ toHtml aHref
  editArticleForm acc aId
  deleteArticleForm acc aId
  p $ a ! href (routeWithAccesstokenToAttribute articlesRoute acc) $ "Back to articles list"

editArticleDetails :: Accesstoken -> Article -> Html
editArticleDetails acc (Article aId aTitle aHref _) = do
  h1 "Edit article"
  H.form ! action (routeWithAccesstokenAndIdToAttribute articleWithIdRoute acc aId) ! method "POST" $ do
    input ! type_ "text" ! name "title" ! value (lazyTextValue aTitle)
    p $ toHtml aHref
    input ! type_ "hidden" ! name "action" ! value "PATCH"
    input ! type_ "submit" ! name "submit" ! value "Save article"
  p $ a ! href (routeWithAccesstokenToAttribute collectionWithAccesstokenRoute acc) $ "Back to articles list"

newArticleForm :: Accesstoken -> Html
newArticleForm acc =
  H.form ! action (routeWithAccesstokenToAttribute articlesRoute acc) ! method "POST" $ do
    input ! type_ "text" ! name "href" ! placeholder "URL"
    input ! type_ "submit" ! name "submit"

newCollectionForm :: Html
newCollectionForm =
  H.form ! action (lazyTextValue collectionRoute) ! method "POST" $ do
    input ! type_ "submit" ! name "submit" ! value "Create new collection"

deleteCollectionForm :: Accesstoken -> Html
deleteCollectionForm acc =
  H.form ! action (routeWithAccesstokenToAttribute collectionWithAccesstokenRoute acc) ! method "POST" $ do
    input ! type_ "hidden" ! name "action" ! value "DELETE"
    input ! type_ "submit" ! name "submit" ! value "Delete collection"

editArticleForm :: Accesstoken -> SqliteUUID -> Html
editArticleForm acc aId =
  H.form ! action (routeWithAccesstokenAndIdToAttribute editArticleWithIdRoute acc aId) ! method "GET" $ do
    button ! type_ "submit" ! class_ "link" $ H.span "Edit article"

--input ! type_ "submit" ! name "submit" ! value "Edit article"

deleteArticleForm :: Accesstoken -> SqliteUUID -> Html
deleteArticleForm acc aId =
  H.form ! action (routeWithAccesstokenAndIdToAttribute articleWithIdRoute acc aId) ! method "POST" $ do
    input ! type_ "hidden" ! name "action" ! value "DELETE"
    button ! type_ "submit" ! class_ "link" $ H.span "Delete article"

--input ! type_ "submit" ! name "submit" ! value "Delete article"

appHeader :: Html
appHeader = do
  H.header $ do
    ""

appFooter :: Html
appFooter = do
  H.footer $ do
    p "Elegance through simplicity"

-- STYLES

renderAppStylesheet :: LText
renderAppStylesheet = C.render appStylesheet

appStylesheet :: C.Css
appStylesheet = do
  htmlStyle
  bodyStyle
  headerStyle
  mainStyle
  footerStyle
  pStyle
  itemStyle

htmlStyle :: C.Css
htmlStyle =
  C.html C.? do
    C.height $ C.pct 100
    C.fontSize $ C.pct 120

bodyStyle :: C.Css
bodyStyle =
  C.body C.? do
    C.padding C.nil C.nil C.nil C.nil
    C.margin C.nil C.nil C.nil C.nil
    C.minHeight $ C.pct 100
    C.textRendering C.optimizeLegibility
    C.display C.grid
    "grid-template-areas"
      C.-: "            \n\
           \ \"header\" \n\
           \ \"main\"   \n\
           \ \"footer\"   "
    "grid-template-columns" C.-: "1fr"
    "grid-template-rows" C.-: "auto 1fr auto"

headerStyle :: C.Css
headerStyle =
  C.header C.? do
    "grid-area" C.-: "header"
    C.margin (C.em 2) C.auto C.nil C.auto
    C.width $ C.pct 80
    C.maxWidth $ C.em 34

mainStyle :: C.Css
mainStyle =
  C.main_ C.? do
    "grid-area" C.-: "main"
    C.color $ C.rgb 51 51 51
    C.width $ C.pct 80
    C.maxWidth $ C.em 38
    C.margin C.nil C.auto (C.em 2) C.auto

footerStyle :: C.Css
footerStyle =
  C.footer C.? do
    "grid-area" C.-: "footer"
    C.textAlign C.center
    C.color C.white
    C.backgroundColor $ C.rgb 17 17 17

pStyle :: C.Css
pStyle =
  C.p C.? do
    C.lineHeight $ C.unitless 1.2

itemStyle :: C.Css
itemStyle = do
  ".item" C.? do
    C.border C.dotted (C.px 1) C.grey
    C.marginBottom $ C.px 20
    C.padding (C.px 5) (C.px 5) (C.px 5) (C.px 5)
  ".item-title" C.? C.a C.? do
    C.textDecoration C.none
  ".item-meta" C.? do
    C.display C.flex
    C.flexFlow C.row CF.wrap
    C.justifyContent C.spaceBetween
    C.fontSize $ C.em 0.8
    C.ul C.? do
      C.marginTop (C.px 5)
      "margin-block-end" C.-: "0px"
    C.li C.? C.display C.inline
  ".item-meta-icons" C.? do
    C.li C.? C.marginRight (C.px 5)
    C.form C.? do
      C.display C.inline
      C.button C.? do
        C.overflow C.visible
        C.width C.auto
      ".link" C.? do
        C.textAlign $ C.alignSide C.sideLeft
        C.color C.blue
        C.margin C.nil C.nil C.nil C.nil
        C.padding C.nil C.nil C.nil C.nil
        C.cursor C.pointer
        "border" C.-: "none"
        C.backgroundImage C.none
        C.backgroundColor C.white

-- UTILS

prettyDate :: UTCTime -> LText
prettyDate = toLText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

routeWithAccesstokenToAttribute :: (LText -> LText) -> Accesstoken -> AttributeValue
routeWithAccesstokenToAttribute route = lazyTextValue . route . uuidToLText . coerce

routeWithAccesstokenAndIdToAttribute :: (LText -> LText -> LText) -> Accesstoken -> SqliteUUID -> AttributeValue
routeWithAccesstokenAndIdToAttribute route acc uuid =
  let newRoute = flip route (uuidToLText $ coerce uuid)
   in routeWithAccesstokenToAttribute newRoute acc

uuidToLText :: UUID -> LText
uuidToLText = toLText . Data.UUID.toText
