module Test.Unit
  ( specs
  ) where


import           Test.Tasty                                           ( TestTree
                                                                      , testGroup
                                                                      )
import           Test.Tasty.Hspec                                     ( testSpec )


import           Test.Domain.Article                                  ( articleSpec )
import           Test.Domain.ArticleList                              ( articleListSpec )
import           Test.Domain.Authorization                            ( authorizationSpec )
import           Test.Domain.Capability                               ( capabilitySpec )
import           Test.Domain.CapabilityList                           ( capabilityListSpec )
import           Test.Domain.Uri                                      ( uriSpec )

specs :: IO TestTree
specs = do
  article        <- testSpec "ArticleSpec" articleSpec
  articleList    <- testSpec "ArticleListSpec" articleListSpec
  authorization  <- testSpec "AuthorizationSpec" authorizationSpec
  capability     <- testSpec "CapabilitySpec" capabilitySpec
  capabilityList <- testSpec "CapabilityListSpec" capabilityListSpec
  uri            <- testSpec "UriSpec" uriSpec
  pure $ testGroup "Unit tests" [article, articleList, authorization, capability, capabilityList, uri]
