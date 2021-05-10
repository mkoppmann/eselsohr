module Lib.Core.Domain.Capability
  ( Capability(..)
  , ObjectReference(..)
  , OverviewPerms(..)
  , ArticlesPerms(..)
  , ArticlePerms(..)
  , Permission(..)
  , defaultOverviewPerms
  , defaultArticlesPermissions
  , defaultArticlePermissions
  ) where

import           Codec.Serialise.Class          ( Serialise )
import           Lib.Core.Domain.Article        ( Article )
import           Lib.Core.Domain.ExpirationDate ( ExpirationDate )
import           Lib.Core.Domain.Id             ( Id )

data Capability = Capability
  { objectRef         :: !ObjectReference
  , petname           :: !(Maybe Text)
  , capExpirationDate :: !(Maybe ExpirationDate)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

instance Ord Capability where
  compare a b = compare (capExpirationDate a) (capExpirationDate b)

data ObjectReference
  = OverviewRef !OverviewPerms
  | ArticlesRef !ArticlesPerms
  | ArticleRef !(Id Article) !ArticlePerms
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data OverviewPerms = OverviewPerms
  { opViewUnlockLinks   :: !Permission
  , opCreateUnlockLinks :: !Permission
  , opDeleteUnlockLinks :: !Permission
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data ArticlesPerms = ArticlesPerms
  { aspViewArticles   :: !Permission
  , aspCreateArticles :: !Permission
  , aspChangeTitles   :: !Permission
  , aspChangeStates   :: !Permission
  , aspDelete         :: !Permission
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data ArticlePerms = ArticlePerms
  { apViewArticle :: !Permission
  , apChangeTitle :: !Permission
  , apChangeState :: !Permission
  , apDelete      :: !Permission
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

data Permission = NotAllowed | Allowed
  deriving stock (Eq, Generic, Show)
  deriving anyclass Serialise

defaultOverviewPerms :: OverviewPerms
defaultOverviewPerms = OverviewPerms Allowed Allowed Allowed

defaultArticlesPermissions :: ArticlesPerms
defaultArticlesPermissions =
  ArticlesPerms Allowed Allowed Allowed Allowed Allowed

defaultArticlePermissions :: ArticlePerms
defaultArticlePermissions = ArticlePerms Allowed Allowed Allowed Allowed
