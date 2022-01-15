module Test.Property
  ( props
  ) where

import           Test.Tasty                                           ( TestTree
                                                                      , testGroup
                                                                      )
import           Test.Tasty.Hedgehog                                  ( fromGroup )

import           Test.Domain.Id                                       ( idProps )
import           Test.Domain.NonEmptyText                             ( nonEmptyTextProps )
import           Test.Ui.Web.Dto.Accesstoken                          ( accesstokenProps )

props :: TestTree
props = testGroup "Property-based tests" [ids, nonEmptyText, accesstoken]
 where
  ids          = fromGroup idProps
  nonEmptyText = fromGroup nonEmptyTextProps
  accesstoken  = fromGroup accesstokenProps
