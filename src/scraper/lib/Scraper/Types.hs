{-# LANGUAGE OverloadedStrings #-}
module Scraper.Types(
  Entry(..),
  getEntries,
  saveOutagesToFolder,
  parseDate
) where

import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value,
                                             defaultOptions, fieldLabelModifier,
                                             genericParseJSON, genericToJSON)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Aeson.Types           (Options)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Foldable              (for_)
import           Data.Proxy                 (Proxy (..))
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             parseTimeM)
import           GHC.Generics               (Generic)
import           Network.HTTP.Req           (GET (..), JsonResponse,
                                             NoReqBody (..), basicAuth,
                                             defaultHttpConfig, https,
                                             queryParam, req, responseBody,
                                             runReq, (/:))
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            ((</>))

data Entry =
  Entry
    { _id                   :: Integer
    , _operatorId           :: Integer
    , _subsection           :: Maybe String
    , _type                 :: Integer
    , _origin               :: Integer
    , _geoType              :: Integer
    , _dateStart            :: String
    , _dateEnd              :: String
    , _lastUpdate           :: String
    , _postalCode           :: String
    , _city                 :: String
    , _district             :: Maybe String
    , _street               :: Maybe String
    , _radius               :: Maybe Double
    , _CoordinateSystemType :: String
    , _coordinates          :: Maybe String
    , _liveInfo             :: Maybe String
    , _comments             :: Maybe String
    , _definition           :: Maybe String
    , _social               :: Maybe String
    , _socialText           :: Maybe String
    , _isOutageInArea       :: Bool
    , _isFixed              :: Bool
    , _SectorType           :: Integer
    , _operatorName         :: String
    , _containerShape       :: Maybe Value
    , _idPublic             :: Maybe Value
    , _internal             :: Integer
    , _houseNr              :: Maybe Value
    , _photo                :: Maybe Value
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Entry where
  toJSON = genericToJSON opts

instance FromJSON Entry where
  parseJSON = genericParseJSON opts

opts :: Options
opts = defaultOptions{fieldLabelModifier = drop 1}

getEntries :: IO (JsonResponse [Entry])
getEntries = runReq defaultHttpConfig $ do
  let auth = basicAuth "frontend" "frontend" -- :D
      url = https "api-public.stoerungsauskunft.de" /: "api" /: "v1" /: "public" /: "outages"
  req GET url NoReqBody Proxy (queryParam "SectorType" (Just (1 :: Int)) <> auth)

saveOutagesToFolder :: FilePath -> IO ()
saveOutagesToFolder fp = do
  entries <- responseBody <$> getEntries
  for_ entries $ \entry@Entry{_id, _dateStart} -> do
    let targetFolder = case parseDate _dateStart of
          Nothing -> "unknown"
          Just d  -> folderName d
        targetFile = fp </> targetFolder </> show _id <> ".json"
    createDirectoryIfMissing True (fp </> targetFolder)
    LBS.writeFile targetFile $ encodePretty entry

parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%m/%d/%Y %T"

folderName :: UTCTime -> String
folderName = formatTime defaultTimeLocale "%F"
