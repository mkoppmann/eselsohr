{-# LANGUAGE OverloadedStrings #-}

module Values (valueTests) where

import Data.Char (chr)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Eselsohr.Article
import Eselsohr.Id
import Eselsohr.WebUrl
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
    ( Gen
    , QuickCheckTests (..)
    , chooseInt
    , elements
    , forAll
    , listOf
    , resize
    , testProperty
    , vectorOf
    , (===)
    )

valueTests :: TestTree
valueTests = localOption (QuickCheckTests 500) $ testGroup "Values" [idTests, titleTests, urlTests]

idTests :: TestTree
idTests = testGroup "IDs" $ map checkKind kinds
  where
    kinds =
        [ ("collection", fmap renderCollectionId . parseCollectionId)
        , ("article", fmap renderArticleId . parseArticleId)
        , ("capability", fmap renderCapabilityId . parseCapabilityId)
        ]
    checkKind (name, parseRender) =
        testGroup
            name
            [ testCase "all standard variant bits" $
                mapM_ (\v -> let input = "123e4567-e89b-42d3-" <> v <> "456-426614174000" in parseRender input @?= Right input) ["8", "9", "a", "b"]
            , testCase "reject malformed or noncanonical IDs" $
                mapM_ (\input -> parseRender input @?= Left InvalidId) invalidIds
            , testProperty "generated UUIDv4 round-trips" $ forAll genId $ \input -> parseRender input === Right input
            , testProperty "arbitrary text is rejected or renders unchanged" $ forAll genInput $ \input ->
                either (const True) (== input) (parseRender input)
            ]
    invalidIds =
        [ ""
        , "123e4567-e89b-42d3-a456-42661417400"
        , "123e4567-e89b-42d3-a456-4266141740000"
        , "123E4567-E89B-42D3-A456-426614174000"
        , "123e4567_e89b-42d3-a456-426614174000"
        , "123e4567-e89b-42d3-a456-42661417400g"
        , "123e4567-e89b-12d3-a456-426614174000"
        , "123e4567-e89b-72d3-a456-426614174000"
        , "123e4567-e89b-42d3-7456-426614174000"
        , "123e4567-e89b-42d3-c456-426614174000"
        , "00000000-0000-0000-0000-000000000000"
        , " 123e4567-e89b-42d3-a456-426614174000"
        , "123e4567-e89b-42d3-a456-426614174000\n"
        , Text.replicate 9000 "a"
        ]

titleTests :: TestTree
titleTests =
    testGroup
        "Title"
        [ testCase "empty" $ mkTitle "" @?= Left EmptyTitle
        , testCase "whitespace only" $ mkTitle (Text.pack unicodeWhitespace) @?= Left EmptyTitle
        , testCase "normalize every Unicode whitespace character" $
            mapM_ (\c -> (renderTitle <$> mkTitle ("  a" <> Text.singleton c <> "b  ")) @?= Right "a b") unicodeWhitespace
        , testCase "collapse runs" $ (renderTitle <$> mkTitle " \ta\n\r  b \x85\x2028 c\x2029 ") @?= Right "a b c"
        , testCase "reject other control characters" $
            mapM_ (\c -> mkTitle ("a" <> Text.singleton c <> "b") @?= Left InvalidTitleCharacter) otherControls
        , testCase "preserve case, combining marks, and format characters" $
            (renderTitle <$> mkTitle "E\x301\x200b") @?= Right "E\x301\x200b"
        , testCase "500 Unicode code points" $
            (renderTitle <$> mkTitle (Text.replicate 500 "😀")) @?= Right (Text.replicate 500 "😀")
        , testCase "501 Unicode code points" $ mkTitle (Text.replicate 501 "😀") @?= Left TitleTooLong
        , testCase "raw input boundary" $
            (renderTitle <$> mkTitle (Text.replicate 3596 " " <> Text.replicate 500 "a")) @?= Right (Text.replicate 500 "a")
        , testCase "oversized raw input" $ mkTitle (Text.replicate 4097 " ") @?= Left TitleInputTooLong
        , testProperty "generated titles round-trip" $ forAll genTitle $ \input ->
            case mkTitle input of
                Left _ -> False
                Right title -> mkTitle (renderTitle title) == Right title
        , testProperty "arbitrary input is rejected or normalizes idempotently" $ forAll genInput $ \input ->
            case mkTitle input of
                Left _ -> True
                Right title -> mkTitle (renderTitle title) == Right title && Text.length (renderTitle title) <= 500
        ]
  where
    otherControls = filter (`notElem` unicodeWhitespace) $ map chr ([0 .. 31] ++ [127 .. 159])

urlTests :: TestTree
urlTests =
    testGroup
        "WebUrl"
        [ testGroup "accepted" $
            [ testCase (Text.unpack input) $ (renderWebUrl <$> parseWebUrl input) @?= Right expected
            | (input, expected) <- validUrls
            ]
        , testCase "reject malformed URLs" $
            mapM_ (\input -> assertBool ("accepted " ++ show input) (isLeft (parseWebUrl input))) invalidUrls
        , testCase "reject literal whitespace and controls"
            $ mapM_ (\c -> assertBool ("accepted character " ++ show c) (isLeft (parseWebUrl ("https://example.com/a" <> Text.singleton c))))
            $ unicodeWhitespace ++ map chr ([0 .. 31] ++ [127 .. 159])
        , testCase "input length boundary" $
            let input = "https://example.com/" <> Text.replicate (8192 - 20) "a"
             in (renderWebUrl <$> parseWebUrl input) @?= Right input
        , testCase "oversized input" $
            (renderWebUrl <$> parseWebUrl (Text.replicate 8193 "a")) @?= Left UrlTooLong
        , testCase "encoded length boundary" $
            let input = "https://example.com/" <> Text.replicate 1362 "é"
                expected = "https://example.com/" <> Text.replicate 1362 "%C3%A9"
             in (renderWebUrl <$> parseWebUrl input) @?= Right expected
        , testCase "oversized encoded output" $
            (renderWebUrl <$> parseWebUrl ("https://example.com/" <> Text.replicate 1363 "é")) @?= Left UrlTooLong
        , testProperty "generated URLs round-trip" $ forAll genUrl $ \input ->
            case parseWebUrl input of
                Left _ -> False
                Right url -> parseWebUrl (renderWebUrl url) == Right url
        , testProperty "arbitrary input is rejected or round-trips" $ forAll genInput $ \input ->
            case parseWebUrl input of
                Left _ -> True
                Right url -> parseWebUrl (renderWebUrl url) == Right url
        ]

validUrls :: [(Text, Text)]
validUrls =
    [ ("https://example.com", "https://example.com")
    , ("HTTP://EXAMPLE.COM:80/a", "http://example.com/a")
    , ("https://Example.COM:00443/", "https://example.com/")
    , ("https://Example.COM./", "https://example.com./")
    , ("https://bücher.example/straße?q=ä#über", "https://xn--bcher-kva.example/stra%C3%9Fe?q=%C3%A4#%C3%BCber")
    , ("https://BU\x308\&CHER.example/", "https://xn--bcher-kva.example/")
    , ("https://b%C3%BCcher.example/", "https://xn--bcher-kva.example/")
    , ("https://faß.de/", "https://xn--fa-hia.de/")
    , ("https://example.com/😀", "https://example.com/%F0%9F%98%80")
    , ("http://localhost/", "http://localhost/")
    , ("http://192.168.0.1/", "http://192.168.0.1/")
    , ("http://[::1]:80/", "http://[::1]/")
    , ("https://[2001:DB8::1]/", "https://[2001:db8::1]/")
    , ("https://example.com/a//b/../c?x=1&x=2&q=a+b#part", "https://example.com/a//b/../c?x=1&x=2&q=a+b#part")
    , ("https://example.com/%2f/%FF?x=%26%3d&empty=#%23", "https://example.com/%2f/%FF?x=%26%3d&empty=#%23")
    , ("https://example.com/?#", "https://example.com/?#")
    ]

invalidUrls :: [Text]
invalidUrls =
    [ ""
    , "/relative"
    , "//example.com/"
    , "example.com"
    , "ftp://example.com/"
    , "javascript:alert(1)"
    , "https:"
    , "https:/example.com/"
    , "https://"
    , "https:///path"
    , "https://./"
    , "https://user:password@example.com/"
    , "https://@example.com/"
    , "http://example.com:443/"
    , "https://example.com:80/"
    , "https://example.com:8080/"
    , "https://example.com:0/"
    , "https://example.com:/"
    , "https://example.com:65536/"
    , "https://example.com:999999999999999999999999999/"
    , "https://example.com:abc/"
    , "https://example.com/%"
    , "https://example.com/%0"
    , "https://example.com/%gg"
    , "https://example.com/\\path"
    , "https://example.com/<path>"
    , " https://example.com/"
    , "https://a..example/"
    , "https://-host.example/"
    , "https://host-.example/"
    , "https://xn--.example/"
    , "https://😀.example/"
    , "https://a_b.example/"
    , "https://a\x3002\&example/"
    , "https://a\x200d\&b.example/"
    , "https://%FF.example/"
    , "https://%2f.example/"
    , "https://%5c097.example/"
    , "https://[::1/"
    , "https://[not-ipv6]/"
    , "https://[v1.a]/"
    , "https://[::1%25lo]/"
    , "https://" <> Text.replicate 64 "a" <> ".example/"
    , "https://" <> Text.intercalate "." (replicate 4 (Text.replicate 63 "a")) <> "/"
    ]

unicodeWhitespace :: [Char]
unicodeWhitespace = map chr $ [9 .. 13] ++ [32, 133, 160, 5760] ++ [8192 .. 8202] ++ [8232, 8233, 8239, 8287, 12288]

genId :: Gen Text
genId = Text.intercalate "-" <$> sequence [hex 8, hex 4, ("4" <>) <$> hex 3, (<>) <$> elements ["8", "9", "a", "b"] <*> hex 3, hex 12]
  where
    hex n = Text.pack <$> vectorOf n (elements (['0' .. '9'] ++ ['a' .. 'f']))

genTitle :: Gen Text
genTitle = do
    count <- chooseInt (1, 250)
    words' <- vectorOf count (elements ["a", "字", "😀"])
    space <- Text.singleton <$> elements unicodeWhitespace
    pure (space <> Text.intercalate space words' <> space)

genUrl :: Gen Text
genUrl = do
    host <- elements ["example.com", "bücher.example", "xn--bcher-kva.example", "localhost", "127.0.0.1", "[::1]"]
    scheme <- elements ["http://", "https://"]
    suffix <- Text.concat <$> resize 30 (listOf (elements ["a", "字", "😀", "%2f", "%26", "/", "?", "=", "&", "+"]))
    pure (scheme <> host <> "/" <> suffix <> "#fragment")

genInput :: Gen Text
genInput = do
    count <- chooseInt (0, 9000)
    Text.pack <$> vectorOf count (chr <$> chooseInt (0, 0x10ffff))
