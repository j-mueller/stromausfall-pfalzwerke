{-| CLI Commands
-}
module Scraper.Command(
  Command(..),
  parseCommand
) where

import           Options.Applicative (CommandFields, Mod, Parser, command,
                                      fullDesc, help, info, long, metavar,
                                      progDesc, strOption, subparser)

data Command =
  Scrape
  | Export{outFile :: FilePath}
  deriving (Eq, Show)

parseCommand :: Parser Command
parseCommand =
  subparser
  $ mconcat
    [ parseScrape
    , export
    ]

parseScrape :: Mod CommandFields Command
parseScrape = command "scrape" $
  info
    (pure Scrape)
    (fullDesc <> progDesc "Scrape the data from the API")

export :: Mod CommandFields Command
export = command "export" $
  info
    (Export <$> parseOutputFile)
    (fullDesc <> progDesc "Export the scraped data to a SQLite db")

parseOutputFile :: Parser FilePath
parseOutputFile =
   strOption
       ( long "sqlite-file" <>
         metavar "FILE" <>
         help "Name of the sqlite db with the results"
       )
