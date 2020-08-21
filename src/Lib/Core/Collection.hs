module Lib.Core.Collection
  ( Collection (..),
  )
where

import Lib.Core.Article (Article)
import Lib.Core.Id (Id)

data Collection = Collection
  { collectionId :: !(Id Collection),
    collectionArticles :: ![Article]
  }
