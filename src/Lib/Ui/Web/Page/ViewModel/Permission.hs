module Lib.Ui.Web.Page.ViewModel.Permission
  ( OverviewPermsVm(..)
  , toOverviewPermsVm
  , ArticlesPermsVm(..)
  , toArticlesPermsVm
  , ArticlePermsVm(..)
  , toArticlePermsVm
  ) where

import           Lib.Domain.Capability                                ( ArticlePerms(..)
                                                                      , ArticlesPerms(..)
                                                                      , OverviewPerms(..)
                                                                      )

data OverviewPermsVm = OverviewPermsVm
  { viewUnlockLinksPermVm   :: !Bool
  , createUnlockLinksPermVm :: !Bool
  , deleteUnlockLinksPermVm :: !Bool
  , shareUnlockLinksPermVm  :: !Bool
  }

toOverviewPermsVm :: OverviewPerms -> OverviewPermsVm
toOverviewPermsVm OverviewPerms {..} = do
  let viewUnlockLinksPermVm   = isJust viewUnlockLinksPerm
      createUnlockLinksPermVm = isJust createUnlockLinksPerm
      deleteUnlockLinksPermVm = isJust deleteUnlockLinksPerm
      shareUnlockLinksPermVm  = isJust shareUnlockLinksPerm
  OverviewPermsVm { .. }

data ArticlesPermsVm = ArticlesPermsVm
  { viewArticlesPermVm     :: !Bool
  , createArticlesPermVm   :: !Bool
  , changeTitlesPermVm     :: !Bool
  , changeStatesPermVm     :: !Bool
  , deleteArticlesPermVm   :: !Bool
  , shareArticleListPermVm :: !Bool
  }

toArticlesPermsVm :: ArticlesPerms -> ArticlesPermsVm
toArticlesPermsVm ArticlesPerms {..} = do
  let viewArticlesPermVm     = isJust viewArticlesPerm
      createArticlesPermVm   = isJust createArticlesPerm
      changeTitlesPermVm     = isJust changeTitlesPerm
      changeStatesPermVm     = isJust changeStatesPerm
      deleteArticlesPermVm   = isJust deleteArticlesPerm
      shareArticleListPermVm = isJust shareArticleListPerm
  ArticlesPermsVm { .. }

data ArticlePermsVm = ArticlePermsVm
  { viewArticlePermVm   :: !Bool
  , changeTitlePermVm   :: !Bool
  , changeStatePermVm   :: !Bool
  , deleteArticlePermVm :: !Bool
  , shareArticlePermVm  :: !Bool
  }

toArticlePermsVm :: ArticlePerms -> ArticlePermsVm
toArticlePermsVm ArticlePerms {..} = do
  let viewArticlePermVm   = isJust viewArticlePerm
      changeTitlePermVm   = isJust changeTitlePerm
      changeStatePermVm   = isJust changeStatePerm
      deleteArticlePermVm = isJust deleteArticlePerm
      shareArticlePermVm  = isJust shareArticlePerm
  ArticlePermsVm { .. }
