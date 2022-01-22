module Lib.Ui.Cli.Command
  ( createCollection
  ) where

import qualified Lib.App.Command                                     as Command
import qualified Lib.Ui.Dto.Accesstoken                              as Accesstoken

import           Lib.App.Port                                         ( MonadRandom )
import           Lib.Domain.Repo.Collection                           ( CollectionRepo )

createCollection :: (CollectionRepo m, MonadRandom m) => m Text
createCollection = do
  (colId, capId) <- Command.createCollection
  let acc = Accesstoken.mkAccesstoken $ Accesstoken.Reference colId capId
  pure $ Accesstoken.encodeToBase32 acc
