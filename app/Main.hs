module Main where

import           Options.Applicative                                  ( (<**>)
                                                                      , (<|>)
                                                                      , CommandFields
                                                                      , Mod
                                                                      , Parser
                                                                      , ParserInfo
                                                                      , ParserPrefs(..)
                                                                      , command
                                                                      , commandGroup
                                                                      , customExecParser
                                                                      , defaultPrefs
                                                                      , execParser
                                                                      , fullDesc
                                                                      , header
                                                                      , help
                                                                      , helper
                                                                      , hidden
                                                                      , info
                                                                      , long
                                                                      , optional
                                                                      , prefs
                                                                      , progDesc
                                                                      , short
                                                                      , showHelpOnError
                                                                      , strOption
                                                                      , subparser
                                                                      )

import qualified Cli
import qualified Lib

import           Cli                                                  ( CliAction
                                                                      , CollectionCommand
                                                                      )

data Options = Options
  { envPath :: !(Maybe FilePath)
  , action  :: !CliAction
  }

options :: ParserInfo Options
options = info (parser <**> helper) (fullDesc <> progDesc "Run Eselsohr" <> header "Eselsohr")
 where
  command :: Parser CliAction
  command = commandsP <|> runServerP

  parser :: Parser Options
  parser = Options <$> configP <*> command

main :: IO ()
main = do
  Options mConfPath action <- customExecParser preferences options
  Lib.main mConfPath action
 where
  preferences :: ParserPrefs
  preferences = defaultPrefs { prefDisambiguate = True, prefShowHelpOnError = True }

------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------

runServerP :: Parser CliAction
runServerP = pure Cli.RunServer

configP :: Parser (Maybe FilePath)
configP = optional . strOption $ long "config-file" <> short 'c' <> help "Path to the env config file"

commandsP :: Parser CliAction
commandsP = longSubParser <|> shortSubParser
 where
  longSubParser :: Parser CliAction
  longSubParser = subparser (collectionCommand <> migrateCommand)

  shortSubParser :: Parser CliAction
  shortSubParser =
    subparser (commandGroup "Short commands:" <> hidden <> collectionShortCommand <> migrateShortCommand)

  migrateCommand :: Mod CommandFields CliAction
  migrateCommand = command "migrate" migrateParserInfo

  migrateShortCommand :: Mod CommandFields CliAction
  migrateShortCommand = command "m" migrateParserInfo

  migrateParserInfo :: ParserInfo CliAction
  migrateParserInfo = info (pure Cli.Migrate) (progDesc "Migrate collections")

  collectionCommand :: Mod CommandFields CliAction
  collectionCommand = command "collection" collectionParserInfo

  collectionShortCommand :: Mod CommandFields CliAction
  collectionShortCommand = command "c" collectionParserInfo

  collectionParserInfo :: ParserInfo CliAction
  collectionParserInfo = info (Cli.Collection <$> collectionP) (progDesc "Manage collections")

  collectionP :: Parser CollectionCommand
  collectionP = subparser $ command "new" (info (pure Cli.NewCollection) (progDesc "Create new collection"))
