module Main where

import qualified Lib
import           Options.Applicative            ( (<**>)
                                                , optional
                                                )
import           Options.Applicative.Builder    ( fullDesc
                                                , header
                                                , help
                                                , info
                                                , long
                                                , progDesc
                                                , short
                                                , strOption
                                                )
import           Options.Applicative.Common     ( Parser
                                                , ParserInfo
                                                )
import           Options.Applicative.Extra      ( execParser
                                                , helper
                                                )

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
