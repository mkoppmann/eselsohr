module Lib.Ui.Cli.Handler
  ( CliAction(..)
  , CollectionCommand(..)
  , commandHandler
  , runCli
  ) where

import qualified Lib.Ui.Cli.Command                                  as Command

import           Lib.App.Port                                         ( MonadRandom )
import           Lib.Domain.Repo.Collection                           ( CollectionRepo )
import           Lib.Infra.Log                                        ( runAppLogIO_ )
import           Lib.Infra.Monad                                      ( AppEnv )

data CliAction
  = RunServer
  | Migrate
  | Collection !CollectionCommand

data CollectionCommand = NewCollection

commandHandler :: (CollectionRepo m, MonadRandom m, MonadIO m) => CliAction -> m ()
commandHandler RunServer                = pass
commandHandler Migrate                  = pass
commandHandler (Collection colCommands) = case colCommands of
  NewCollection -> do
    acc <- Command.createCollection
    print $ "Accesstoken: " <> acc

runCli :: AppEnv -> CliAction -> IO ()
runCli env = runAppLogIO_ env . commandHandler
