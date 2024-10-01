{-| CLI Commands
-}
module Scraper.Command(
  Command(..),
  parseScrape
) where

import           Options.Applicative (Parser, help, metavar, strArgument, value)

newtype Command =
  Scrape{outFile :: FilePath}
  deriving (Eq, Show)

parseScrape :: Parser Command
parseScrape = Scrape <$> parseOutputFile

parseOutputFile :: Parser FilePath
parseOutputFile =
  strArgument
    ( metavar "FILE" <>
      help "Name of the sqlite db with the results" <>
      value "outages.sqlite"
    )
