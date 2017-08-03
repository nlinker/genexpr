{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative (ParserInfo, execParser, fullDesc, header,
                                      help, helper, info, infoOption, long,
                                      metavar, progDesc, strOption, value, (<>))

data Config = Config

data Opts = Opts
  { configFile :: !(FilePath)
  }

main :: IO ()
main = do
  opts <- execParser optsParser
  putStrLn ("Hello, " <> show (configFile opts))
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <> progDesc "optparse " <> header "executable - a small example program for optparse-applicative")
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions =
      Opts <$> strOption (long "config-file" <> metavar "VALUE" <> value "default" <> help "Configuration file")
