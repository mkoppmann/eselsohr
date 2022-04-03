module Lib.Ui.Web.Page.Style
  ( app
  ) where

import qualified Clay.Flexbox                                        as CF

import           Clay

app :: Css
app = do
  htmlStyle
  bodyStyle
  headerStyle
  mainStyle
  footerStyle
  pStyle
  itemStyle
  formButtonAsLink
  noBullet
  inputStyle
  fieldsetInputStyle
  formGroupStyle

htmlStyle :: Css
htmlStyle = html ? do
  height $ pct 100
  fontSize $ pct 120

bodyStyle :: Css
bodyStyle = body ? do
  padding nil nil nil nil
  margin nil nil nil nil
  minHeight $ pct 100
  textRendering optimizeLegibility
  display grid
  "grid-template-areas"
    -: "            \n\
         \ \"header\" \n\
         \ \"main\"   \n\
         \ \"footer\"   "
  "grid-template-columns" -: "1fr"
  "grid-template-rows" -: "auto 1fr auto"

headerStyle :: Css
headerStyle = header ? do
  "grid-area" -: "header"
  margin (em 2) auto nil auto
  width $ pct 80
  maxWidth $ em 34

mainStyle :: Css
mainStyle = main_ ? do
  "grid-area" -: "main"
  color $ rgb 51 51 51
  width $ pct 80
  maxWidth $ em 38
  margin nil auto (em 2) auto

footerStyle :: Css
footerStyle = footer ? do
  "grid-area" -: "footer"
  textAlign center
  color white
  backgroundColor $ rgb 17 17 17

pStyle :: Css
pStyle = p ? do
  lineHeight $ unitless 1.2

formButtonAsLink :: Css
formButtonAsLink = do
  button # ".link" ? do
    overflow visible
    width auto
    textAlign $ alignSide sideLeft
    color blue
    margin nil nil nil nil
    padding nil nil nil nil
    cursor pointer
    "border" -: "none"
    backgroundImage none
    backgroundColor white

itemStyle :: Css
itemStyle = do
  ".item" ? do
    border (px 1) dotted grey
    marginBottom $ px 20
    padding (px 5) (px 5) (px 5) (px 5)
  ".item-title" ? a ? do
    textDecoration none
  ".item-meta" ? do
    display flex
    flexFlow row CF.wrap
    justifyContent spaceBetween
    fontSize $ em 0.8
    ul ? do
      marginTop (px 5)
      "margin-block-end" -: "0px"
    li ? display inline
  ".item-meta-icons" ? do
    li ? marginRight (px 5)
    form ? do
      display inline

noBullet :: Css
noBullet = ".no-bullet" ? li ? listStyleType none

inputStyle :: Css
inputStyle = input ? do
  marginTop $ Clay.rem 0.4
  marginBottom $ Clay.rem 0.4

fieldsetInputStyle :: Css
fieldsetInputStyle = fieldset ? input ? do
  marginLeft $ Clay.rem 0.4
  marginRight $ Clay.rem 0.4

formGroupStyle :: Css
formGroupStyle = ".form-group" ? do
  label ? display block
  input ? display block
