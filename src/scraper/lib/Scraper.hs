{-# LANGUAGE OverloadedStrings #-}
module Scraper(
  runScraper
) where

import           Options.Applicative (Parser, customExecParser, disambiguate,
                                      fullDesc, headerDoc, help, helper, info,
                                      long, metavar, prefs, progDesc, short,
                                      showHelpOnEmpty, showHelpOnError,
                                      strOption)
import qualified Scraper.Types

runScraper :: IO ()
runScraper = do
  fp <- customExecParser
          (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
          (info (helper <*> parsePath)
            (fullDesc <> progDesc "The command-line interface for pw-scraper"
            <> headerDoc (Just "pw-scraper")
            )
          )
  putStrLn $ "Writing files to " <> fp
  Scraper.Types.saveOutagesToFolder fp

parsePath :: Parser FilePath
parsePath = strOption (long "path" <> short 'p' <> metavar "FILEPATH" <> help "The directory where the json files will be saved")
