module Test.Domain.Uri
    ( uriSpec
    ) where

import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    )

import qualified Lib.Domain.Error as Error
import qualified Lib.Domain.Uri as Uri

import Lib.Domain.Error (AppErrorType)
import Lib.Domain.Uri (Uri)

uriSpec :: Spec
uriSpec = describe "Lib.Domain.Uri" $ do
    it ("returns an 'Uri' for: " <> toString okUri) $ do
        shouldSucceed Uri.mkUri okUri

    it ("returns an 'Uri' with 'unfilteredUri' for: " <> toString okUri) $ do
        shouldSucceed Uri.unfilteredUri okUri

    it ("returns an 'AppErrorType' for: " <> toString forbiddenPortUri) $ do
        shouldReturnError Uri.mkUri forbiddenPortUri $ one Uri.ForbiddenPort

    it ("returns an 'Uri' with 'unfilteredUri' for: " <> toString forbiddenPortUri) $ do
        shouldSucceed Uri.unfilteredUri forbiddenPortUri

    it ("returns an 'AppErrorType' for: " <> toString forbiddenProtocolUri) $ do
        shouldReturnError Uri.mkUri forbiddenProtocolUri $ one Uri.ForbiddenProtocol

    it ("returns an 'Uri' with 'unfilteredUri' for: " <> toString forbiddenProtocolUri) $ do
        shouldSucceed Uri.unfilteredUri forbiddenProtocolUri

    it ("returns an 'AppErrorType' for: " <> toString forbiddenHostnameUri) $ do
        shouldReturnError Uri.mkUri forbiddenHostnameUri $ one Uri.ForbiddenHostname

    it ("returns an 'Uri' with 'unfilteredUri' for: " <> toString forbiddenHostnameUri) $ do
        shouldSucceed Uri.unfilteredUri forbiddenHostnameUri

    it ("returns an 'AppErrorType' for: " <> toString forbiddenIPv4RangeUri) $ do
        shouldReturnError Uri.mkUri forbiddenIPv4RangeUri $ one Uri.ForbiddenIPv4Range

    it ("returns an 'Uri' with 'unfilteredUri' for: " <> toString forbiddenIPv4RangeUri) $ do
        shouldSucceed Uri.unfilteredUri forbiddenIPv4RangeUri

    it ("returns an 'AppErrorType' for: " <> toString forbiddenIPv6RangeUri) $ do
        shouldReturnError Uri.mkUri forbiddenIPv6RangeUri $ one Uri.ForbiddenIPv6Range

    it ("returns an 'Uri' with 'unfilteredUri' for: " <> toString forbiddenIPv6RangeUri) $ do
        shouldSucceed Uri.unfilteredUri forbiddenIPv6RangeUri

------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------

shouldSucceed :: (Text -> Either AppErrorType Uri) -> Text -> Expectation
shouldSucceed mkUri addr = either errorCase successCase $ mkUri addr
  where
    successCase :: Uri -> Expectation
    successCase = shouldBe addr . toText

    errorCase :: AppErrorType -> Expectation
    errorCase = expectationFailure . errorMsg . show

    errorMsg :: String -> String
    errorMsg err = "Should have returned uri, but got: " <> err

shouldReturnError :: Show a => (Text -> Either AppErrorType Uri) -> Text -> NonEmpty a -> Expectation
shouldReturnError mkUri addr exception = either errorCase successCase $ mkUri addr
  where
    errorCase :: AppErrorType -> Expectation
    errorCase = shouldBe (Error.invalid $ show exception)

    successCase :: Uri -> Expectation
    successCase _uri = expectationFailure "Should have returned error, but got uri: "

okUri :: Text
okUri = "http://www.example.org"

forbiddenPortUri :: Text
forbiddenPortUri = "http://www.example.org:8080"

forbiddenProtocolUri :: Text
forbiddenProtocolUri = "file:///etc/passwd"

forbiddenHostnameUri :: Text
forbiddenHostnameUri = "http://localhost"

forbiddenIPv4RangeUri :: Text
forbiddenIPv4RangeUri = "http://192.168.0.1"

forbiddenIPv6RangeUri :: Text
forbiddenIPv6RangeUri = "http://[::1]"
