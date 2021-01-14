module Lib.Core.Domain.Capability
  ( Capability (..),
    Action (..),
    CommandAction (..),
    DeleteAction (..),
    PatchAction (..),
    PostAction (..),
    QueryAction (..),
    SharedAction (..),
    FrontendAction (..),
    ResourceOverviewActions (..),
    GetArticlesActions (..),
    GetArticleActions (..),
    CreateGetArticlesCapActions (..),
  )
where

import Data.Binary (Binary)
import Data.Time (UTCTime)
import Lib.Core.Domain.Article (Article)
import Lib.Core.Domain.ExpirationDate (ExpirationDate)
import Lib.Core.Domain.Id (Id)

data Capability = Capability
  { -- | A name like “GetArticles read-only for Alice”.
    petname :: !(Maybe Text),
    -- | Date and time at which this 'Capability' is no longer valid.
    capExpirationDate :: !(Maybe ExpirationDate),
    -- | 'Id' of the 'Action' this 'Capability' is pointing to.
    actionId :: !(Id Action)
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Binary)

data Action
  = Command !CommandAction
  | Query !QueryAction
  | Shared !SharedAction
  | Frontend !FrontendAction
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data CommandAction
  = Delete !DeleteAction
  | Patch !PatchAction
  | Post !PostAction
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data DeleteAction
  = -- | Deletes the 'SharedAction' with this 'Action'’s 'Id'.
    DeleteSharedAction !(Id Action)
  | -- | Delete the matching 'GetArticles' 'Capability' stored in the
    -- 'Action' and then deletes itself.
    DeleteGetArticles !(Id Capability)
  | -- | Delete the 'Article'.
    DeleteArticle !(Id Article)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data PatchAction
  = -- | Change the 'Article'’s 'title'.
    ChangeArticleTitle !(Id Article)
  | -- | Set the 'state' of the 'Article' to 'Archived'.
    ArchiveArticle !(Id Article)
  | -- | Set the 'state' of the 'Article' to 'Unread'.
    UnreadArticle !(Id Article)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data CreateGetArticlesCapActions = CreateGetArticlesCapActions
  { cgacGetArticles :: !(Id Action),
    cgacGetActiveGetArticlesCap :: !(Id Action),
    cgacResourceOverview :: !(Id Action)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data PostAction
  = -- | Creates a new 'Resource'.
    CreateResource
  | -- | Creates a new 'SharedAction' with this 'Action'’s 'Id'.
    ShareAction !(Id Action)
  | -- | Unlocks 'Resource' and creates new 'Capability' for 'GetArticles',
    -- and all 'Action's contained in it.
    CreateGetArticlesCap !CreateGetArticlesCapActions
  | -- | Creates a new 'Article' and store it in a 'Resource'.
    -- The 'Action' 'Id' points to the 'GetArticles' 'Action'. where the new
    -- 'GetArticle' 'Action' needs to be added to the 'Set'.
    CreateArticle !(Id Action)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data ResourceOverviewActions = ResourceOverviewActions
  { roaGetSharedActions :: !(Id Action),
    roaGetSharedResourceOverviewAction :: !(Id Action),
    roaGetActiveGetArticlesCap :: !(Id Action),
    roaGetArticles :: !(Id Action),
    roaCreateGetArticlesCap :: !(Maybe (Id Action)),
    roaFrontCreateGetArticlesCap :: !(Maybe (Id Action))
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data GetArticlesActions = GetArticlesActions
  { gaaGetSharedArticlesActions :: !(Id Action),
    gaaCreateArticle :: !(Maybe (Id Action)),
    gaaFrontCreateArticle :: !(Maybe (Id Action)),
    gaaShowArticles :: !(Set (Id Action))
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data GetArticleActions = GetArticleActions
  { gaaShowArticle :: !(Id Action),
    gaaGetSharedArticleAction :: !(Id Action),
    gaaChangeArticleTitle :: !(Maybe (Id Action)),
    gaaArchiveArticle :: !(Maybe (Id Action)),
    gaaUnreadArticle :: !(Maybe (Id Action)),
    gaaDeleteArticle :: !(Maybe (Id Action)),
    gaaGetArticles :: !(Maybe (Id Action))
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data QueryAction
  = -- | Get data for showing the 'Resource' main page.
    ResourceOverview !ResourceOverviewActions
  | -- | Get all open 'GetArticles' capabilities with their corresponding
    -- revoker.
    GetActiveGetArticlesCaps !(Set (Id Capability, Id Capability))
  | -- | Get all 'Article's in the resource.
    GetArticles !GetArticlesActions
  | -- | Get 'Article' in the resource with this 'Id'.
    GetArticle !(Id Article) !GetArticleActions
  | -- | Get all 'Action's that are currently shared.
    -- Contains the 'Set' of 'Id's of 'Action's that are currently shared, with
    -- the matching action to delete this 'SharedAction'.
    GetSharedActions !(Set (Id Action, Id Action))
  | -- | Get all shared 'ResourceOverview' 'Action's.
    -- Contains the 'Set' of 'Id's of 'Action's that are currently shared, with
    -- the matching action to delete this 'SharedAction'.
    GetSharedResourceOverviewAction !(Set (Id Action, Id Action))
  | -- | Get all shared 'GetArticles' 'Action's
    -- Contains the 'Set' of 'Id's of 'Action's that are currently shared, with
    -- the matching action to delete this 'SharedAction'.
    GetSharedArticlesAction !(Set (Id Action, Id Action))
  | -- | Get all shared 'GetArticle' 'Action's.
    -- Contains the 'Set' of 'Id's of 'Action's that are currently shared, with
    -- the matching action to delete this 'SharedAction'.
    GetSharedArticleAction !(Set (Id Action, Id Action))
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data SharedAction = SharedAction
  { -- | Date and time at which this 'SharedAction' is no longer valid.
    sharedExpirationDate :: !(Maybe UTCTime),
    -- | Counter of remaining usages of this 'SharedAction'.
    sharedRemainingUsages :: !(Maybe Natural),
    -- | 'Id' of the 'Action' this 'SharedAction' is pointing to.
    sharedAction :: !(Id Action)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

-- | Represents a flow in the HTML client.
-- HTML forms point to the @api@ route which are handled by the frontend command
-- controller, which then executes the stored 'CommandAction' and then redirects
-- to the link specified in the 'redirectTo' field.
-- The 'redirectTo' field points to a frontend route where the query controller
-- will then execute the 'QueryAction' and render the page with that data.
data FrontendAction = FrontendAction
  { command :: !(Id Action),
    query :: !(Id Action),
    redirectTo :: !Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)
