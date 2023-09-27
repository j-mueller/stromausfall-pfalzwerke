{-# LANGUAGE OverloadedStrings #-}
module Scraper(
  runScraper
) where

import           Options.Applicative (Parser, customExecParser, disambiguate,
                                      fullDesc, headerDoc, help, helper, info,
                                      long, metavar, prefs, progDesc, short,
                                      showHelpOnEmpty, showHelpOnError,
                                      strOption)
import           Scraper.Command     (Command (..), parseCommand)
import           Scraper.Sqlite      (ImportArgs (..), importJSONFiles)
import qualified Scraper.Types

runScraper :: IO ()
runScraper = do
  let p = (,) <$> parsePath <*> parseCommand
  (rootFolder, command) <- customExecParser
          (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
          (info (helper <*> p)
            (fullDesc <> progDesc "The command-line interface for pw-scraper"
            <> headerDoc (Just "pw-scraper")
            )
          )
  case command of
    Scrape -> do
      putStrLn $ "Writing files to " <> rootFolder
      Scraper.Types.saveOutagesToFolder rootFolder
    Export{outFile} -> do
      putStrLn $ "Loading JSON files from " <> rootFolder <> " into " <> outFile
      importJSONFiles ImportArgs{rootFolder, sqliteDbFile=outFile}

parsePath :: Parser FilePath
parsePath = strOption (long "path" <> short 'p' <> metavar "FILEPATH" <> help "The directory where the json files will be saved")
