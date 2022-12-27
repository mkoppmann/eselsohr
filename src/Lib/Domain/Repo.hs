module Lib.Domain.Repo
    ( RepositoryCommand (..)
    , RepositoryCommandSync (..)
    ) where

import Lib.Domain.Collection (Collection)
import Lib.Domain.Id (Id)

data RepositoryCommand m = RepositoryCommand
    { collectionId :: !(Id Collection)
    , action :: !(m ())
    }

data RepositoryCommandSync m = RepositoryCommandSync
    { syncVar :: !(TMVar ())
    , repoCommand :: !(RepositoryCommand m)
    }
