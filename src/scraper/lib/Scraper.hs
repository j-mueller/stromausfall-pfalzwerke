{-# LANGUAGE OverloadedStrings #-}
module Scraper(
  runScraper
) where

import           Options.Applicative (customExecParser, disambiguate, fullDesc,
                                      headerDoc, helper, info, prefs, progDesc,
                                      showHelpOnEmpty, showHelpOnError)
import           Scraper.Command     (Command (..), parseScrape)
import           Scraper.Sqlite      (ImportArgs (..), importEntriesFromAPI)

runScraper :: IO ()
runScraper = do
  command <- customExecParser
          (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
          (info (helper <*> parseScrape)
            (fullDesc <> progDesc "The command-line interface for pw-scraper. Downloads the latest data and saves it to a sqlite database. Existing entries will be updated."
            <> headerDoc (Just "pw-scraper")
            )
          )

  case command of
    Scrape{outFile} -> do
      putStrLn $ "Writing outages data to " <> outFile
      importEntriesFromAPI ImportArgs{sqliteDbFile=outFile}
