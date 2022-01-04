module Main where

import           Options.Applicative            ( (<**>)
                                                , Parser
                                                , ParserInfo
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , optional
                                                , progDesc
                                                , short
                                                , strOption
                                                )

import qualified Lib

newtype ExeConfig = ExeConfig
  { envPath :: Maybe FilePath
  }

config :: Parser ExeConfig
config = ExeConfig <$> optional
  (strOption
    (long "config-file" <> short 'c' <> help "Path to the env config file")
  )

main :: IO ()
main = Lib.main . envPath =<< execParser opts
 where
  opts :: ParserInfo ExeConfig
  opts = info (config <**> helper)
              (fullDesc <> progDesc "Run Eselsohr" <> header "Eselsohr")
