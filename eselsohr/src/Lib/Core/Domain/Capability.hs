module Lib.Core.Domain.Capability
  ( Capability(..)
  , Action(..)
  , CommandAction(..)
  , DeleteAction(..)
  , PatchAction(..)
  , PostAction(..)
  , QueryAction(..)
  , ResourceOverviewActions(..)
  , GetArticlesActions(..)
  , GetArticleActions(..)
  , CreateGetArticlesCapActions(..)
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Lib.Core.Domain.Article        ( Article )
import           Lib.Core.Domain.ExpirationDate ( ExpirationDate )
import           Lib.Core.Domain.Id             ( Id )

data Capability = Capability
  { -- | A name like “GetArticles read-only for Alice”.
    petname           :: !(Maybe Text)
  , -- | Date and time at which this 'Capability' is no longer valid.
    capExpirationDate :: !(Maybe ExpirationDate)
  , -- | 'Id' of the 'Action' this 'Capability' is pointing to.
    actionId          :: !(Id Action)
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass Serialise

data Action
  = Command !CommandAction
  | Query !QueryAction
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Serialise)

data CommandAction
  = Delete !DeleteAction
  | Patch !PatchAction
  | Post !PostAction
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Serialise)

data DeleteAction
  = -- | Delete the matching 'GetArticles' 'Capability' stored in the
    -- 'Action' and then deletes itself.
    DeleteGetArticles !(Id Capability)
  | -- | Delete the 'Article'.
    DeleteArticle !(Id Article)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Serialise)

data PatchAction
  = -- | Change the 'Article'’s 'title'.
    ChangeArticleTitle !(Id Article)
  | -- | Set the 'state' of the 'Article' to 'Archived'.
    ArchiveArticle !(Id Article)
  | -- | Set the 'state' of the 'Article' to 'Unread'.
    UnreadArticle !(Id Article)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Serialise)

data CreateGetArticlesCapActions = CreateGetArticlesCapActions
  { cgacGetArticles             :: !(Id Action)
  , cgacGetActiveGetArticlesCap :: !(Id Action)
  , cgacResourceOverview        :: !(Id Action)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data PostAction
  = -- | Unlocks 'Resource' and creates new 'Capability' for 'GetArticles',
    -- and all 'Action's contained in it.
    CreateGetArticlesCap !CreateGetArticlesCapActions
  | -- | Creates a new 'Article' and store it in a 'Resource'.
    -- The 'Action' 'Id' points to the 'GetArticles' 'Action'. where the new
    -- 'GetArticle' 'Action' needs to be added to the 'Set'.
    CreateArticle !(Id Action)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Serialise)

data ResourceOverviewActions = ResourceOverviewActions
  { roaGetActiveGetArticlesCap :: !(Id Action)
  , roaGetArticles             :: !(Id Action)
  , roaCreateGetArticlesCap    :: !(Maybe (Id Action))
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data GetArticlesActions = GetArticlesActions
  { gaaCreateArticle :: !(Maybe (Id Action))
  , gaaShowArticles  :: !(HashSet (Id Action))
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data GetArticleActions = GetArticleActions
  { gaaShowArticle        :: !(Id Action)
  , gaaChangeArticleTitle :: !(Maybe (Id Action))
  , gaaArchiveArticle     :: !(Maybe (Id Action))
  , gaaUnreadArticle      :: !(Maybe (Id Action))
  , gaaDeleteArticle      :: !(Maybe (Id Action))
  , gaaGetArticles        :: !(Maybe (Id Action))
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data QueryAction
  = -- | Get data for showing the 'Resource' main page.
    ResourceOverview !ResourceOverviewActions
  | -- | Get all open 'GetArticles' capabilities with their corresponding
    -- revoker.
    GetActiveGetArticlesCaps !(HashSet (Id Capability, Id Capability))
  | -- | Get all 'Article's in the resource.
    GetArticles !GetArticlesActions
  | -- | Get 'Article' in the resource with this 'Id'.
    GetArticle !(Id Article) !GetArticleActions
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Serialise)
