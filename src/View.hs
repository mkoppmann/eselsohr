module View where

import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.UUID (toText)
import Model
import Routes
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

collectionMappingList :: [CollectionMapping] -> Html
collectionMappingList collmaps = docTypeHtml $ do
  H.head $ do
    H.title "My collection mappings"
  body $ do
    h2 "Add a new collection"
    newCollectionForm
    h1 "The collection mappings list"
    ul $ mapM_ renderCollMaps collmaps
  where
    renderCollMaps = li . toHtml . prettyCollMap
    prettyCollMap (CollectionMapping collId acc) = do
      let lCollId = show $ getSqliteUUID $ getCollectionId collId :: LText
      p $ a ! href (accesstokenToAttribute collectionWithAccesstokenRoute acc) $ toHtml lCollId
      deleteCollectionForm acc

articleList :: [Article] -> Html
articleList articles = docTypeHtml $ do
  H.head $ do
    H.title "My article list"
  body $ do
    h2 "Add a new article"
    newArticleForm
    h1 "The mighty article list"
    ul $ mapM_ renderArticle articles
  where
    renderArticle = li . toHtml . prettyArticle
    prettyArticle (Article aId aTitle _ created) = do
      p $ do
        toHtml $ prettyDate created <> "| "
        a ! href (uuidToAttribute articleWithIdRoute aId) $ toHtml aTitle
      editArticleForm aId
      deleteArticleForm aId

articleDetails :: Article -> Html
articleDetails (Article aId aTitle aHref created) = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml $ aTitle <> " – Eselsohr"
  body $ do
    h1 $ toHtml aTitle
    p $ toHtml $ "Created: " <> prettyDate created
    p $ a ! href (lazyTextValue aHref) $ toHtml aHref
    editArticleForm aId
    deleteArticleForm aId
    p $ a ! href (lazyTextValue articlesRoute) $ "Back to articles list"

editArticleDetails :: Article -> Html
editArticleDetails (Article aId aTitle aHref _) = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml $ "Edit – " <> aTitle <> " – Eselsohr"
  body $ do
    h1 "Edit article"
    H.form ! action (uuidToAttribute articleWithIdRoute aId) ! method "POST" $ do
      input ! type_ "text" ! name "title" ! value (lazyTextValue aTitle)
      p $ toHtml aHref
      input ! type_ "hidden" ! name "action" ! value "PATCH"
      input ! type_ "submit" ! name "submit" ! value "Save article"
    p $ a ! href (lazyTextValue articlesRoute) $ "Back to articles list"

newArticleForm :: Html
newArticleForm = H.form ! action (lazyTextValue articlesRoute) ! method "POST" $ do
  input ! type_ "text" ! name "href" ! placeholder "URL"
  input ! type_ "submit" ! name "submit"

newCollectionForm :: Html
newCollectionForm = H.form ! action (lazyTextValue collectionCreationRoute) ! method "POST" $ do
  input ! type_ "submit" ! name "submit"

deleteCollectionForm :: Accesstoken -> Html
deleteCollectionForm acc = H.form ! action (accesstokenToAttribute collectionWithAccesstokenRoute acc) ! method "POST" $ do
  input ! type_ "hidden" ! name "action" ! value "DELETE"
  input ! type_ "submit" ! name "submit" ! value "Delete collection"

editArticleForm :: SqliteUUID -> Html
editArticleForm aId = H.form ! action (uuidToAttribute editArticleWithIdRoute aId) ! method "GET" $ do
  input ! type_ "submit" ! name "submit" ! value "Edit article"

deleteArticleForm :: SqliteUUID -> Html
deleteArticleForm aId = H.form ! action (uuidToAttribute articleWithIdRoute aId) ! method "POST" $ do
  input ! type_ "hidden" ! name "action" ! value "DELETE"
  input ! type_ "submit" ! name "submit" ! value "Delete article"

prettyDate :: UTCTime -> LText
prettyDate = toLText . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

collectionIdToAttribute :: (LText -> LText) -> CollectionId -> AttributeValue
collectionIdToAttribute route = uuidToAttribute route . getCollectionId

accesstokenToAttribute :: (LText -> LText) -> Accesstoken -> AttributeValue
accesstokenToAttribute route = uuidToAttribute route . getAccesstoken

uuidToAttribute :: (LText -> LText) -> SqliteUUID -> AttributeValue
uuidToAttribute route = lazyTextValue . route . toLText . Data.UUID.toText . getSqliteUUID
