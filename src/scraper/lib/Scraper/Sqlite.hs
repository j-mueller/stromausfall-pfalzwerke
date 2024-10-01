{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-| Writing data to a sqlite db
-}
module Scraper.Sqlite(
  OutageEntryT(..),
  OutageEntryRow,
  OutageEntryRowId,
  entryToRow,

  -- * Database
  DB(..),
  db,
  dbOutageEntries,
  migrateSqlite,
  migrateSqlitePool,
  imports,

  -- * Etc.
  ImportArgs(..),
  importEntriesFromAPI
) where

import           Control.Concurrent                       (threadDelay)
import           Control.Exception                        (catch, throw)
import           Control.Lens                             (makeLenses)
import           Control.Monad                            (unless, void, when)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Functor.Identity                    (Identity (..))
import           Data.Int                                 (Int32)
import           Data.Pool                                (Pool)
import qualified Data.Pool                                as Pool
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           Data.Time                                (UTCTime (utctDay),
                                                           diffUTCTime)
import           Data.Time.Calendar.OrdinalDate           (toOrdinalDate)
import           Database.Beam                            (Beamable, Columnar,
                                                           Database,
                                                           DatabaseEntity,
                                                           DatabaseSettings,
                                                           SqlInsert, SqlSelect,
                                                           Table (..),
                                                           TableEntity,
                                                           current_,
                                                           defaultDbSettings,
                                                           insertValues,
                                                           runInsert, (<-.))
import qualified Database.Beam                            as Beam
import           Database.Beam.Backend.SQL.BeamExtensions (anyConflict,
                                                           insertOnConflict,
                                                           onConflictUpdateSet)
import           Database.Beam.Backend.SQL.SQL92          (timestampType)
import           Database.Beam.Migrate.Generics           (HasDefaultSqlDataType (..))
import qualified Database.Beam.Migrate.Simple             as Migrate
import           Database.Beam.Sqlite.Connection          (Sqlite,
                                                           runBeamSqlite)
import qualified Database.Beam.Sqlite.Migrate             as Sqlite
import           Database.SQLite.Simple                   (Connection)
import qualified Database.SQLite.Simple                   as Sqlite
import           GHC.Generics                             (Generic)
import           Network.HTTP.Req                         (responseBody)
import qualified Scraper.Types                            as Entry
import           Scraper.Types                            (Entry, getEntries,
                                                           parseDate)
import qualified Streaming                                as S
import           Streaming                                (Stream)
import qualified Streaming.Prelude                        as S
import           Streaming.Prelude                        (Of)
import           System.Directory                         (doesFileExist)

instance HasDefaultSqlDataType Sqlite UTCTime where
  defaultSqlDataType _ _ _ = timestampType Nothing True

data OutageEntryT f
  = OutageEntryT
      { _outageEntryId                   :: Columnar f Int32
      , _outageEntryOperatorId           :: Columnar f Int32
      , _outageEntrySubsection           :: Columnar f (Maybe Text)
      , _outageEntryType                 :: Columnar f Int32
      , _outageEntryorigin               :: Columnar f Int32
      , _outageEntrygeoType              :: Columnar f Int32
      , _outageEntrydateStart            :: Columnar f UTCTime
      , _outageEntrydateEnd              :: Columnar f UTCTime
      , _outageEntryDurationMinutes      :: Columnar f Int32
      , _outageEntrylastUpdate           :: Columnar f Text
      , _outageEntrypostalCode           :: Columnar f Text
      , _outageEntrycity                 :: Columnar f Text
      , _outageEntrydistrict             :: Columnar f (Maybe Text)
      , _outageEntrystreet               :: Columnar f (Maybe Text)
      , _outageEntryradius               :: Columnar f (Maybe Double)
      , _outageEntryCoordinateSystemType :: Columnar f Text
      , _outageEntrycoordinates          :: Columnar f (Maybe Text)
      , _outageEntryliveInfo             :: Columnar f (Maybe Text)
      , _outageEntrycomments             :: Columnar f (Maybe Text)
      , _outageEntrydefinition           :: Columnar f (Maybe Text)
      , _outageEntrysocial               :: Columnar f (Maybe Text)
      , _outageEntrysocialText           :: Columnar f (Maybe Text)
      , _outageEntryisOutageInArea       :: Columnar f Bool
      , _outageEntryisFixed              :: Columnar f Bool
      , _outageEntrySectorType           :: Columnar f Int32
      , _outageEntryoperatorName         :: Columnar f Text
      }
      deriving stock (Generic)
      deriving anyclass (Beamable)

type OutageEntryRow = OutageEntryT Identity

type OutageEntryRowId = PrimaryKey OutageEntryT Identity

instance Table OutageEntryT where
  data PrimaryKey OutageEntryT f = OutageEntryRowId' (Columnar f Int32)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = OutageEntryRowId' . _outageEntryId

data ConversionError =
  FailedToConvert{column :: String, contents :: String, id_ :: Integer}
  | DateStartTooOld{value :: UTCTime, id_ :: Integer}
  deriving Show

entryToRow :: Entry -> Either ConversionError OutageEntryRow
entryToRow entry = do
  _outageEntrydateStart <- maybe (Left FailedToConvert{column="dateStart", contents=Entry._dateStart entry, id_=Entry._id entry}) pure (parseDate (Entry._dateStart entry))
  _outageEntrydateEnd <- maybe (Left FailedToConvert{column="dateEnd", contents=Entry._dateEnd entry, id_=Entry._id entry}) pure (parseDate (Entry._dateEnd entry))
  let _outageEntryDurationMinutes = round $ abs $ (_outageEntrydateEnd `diffUTCTime` _outageEntrydateStart) / 60
  when (fst (toOrdinalDate (utctDay _outageEntrydateStart)) < 2023) $ Left DateStartTooOld{value=_outageEntrydateStart, id_=Entry._id entry}
  pure $
    OutageEntryT
        { _outageEntryId                   = fromIntegral (Entry._id entry)
        , _outageEntryOperatorId           = fromIntegral (Entry._operatorId entry)
        , _outageEntrySubsection           = fmap Text.pack (Entry._subsection entry)
        , _outageEntryType                 = fromIntegral (Entry._type entry)
        , _outageEntryorigin               = fromIntegral (Entry._origin entry)
        , _outageEntrygeoType              = fromIntegral (Entry._geoType entry)
        , _outageEntrydateStart
        , _outageEntrydateEnd
        , _outageEntryDurationMinutes
        , _outageEntrylastUpdate           = Text.pack (Entry._lastUpdate entry)
        , _outageEntrypostalCode           = Text.pack (Entry._postalCode entry)
        , _outageEntrycity                 = Text.pack (Entry._city entry)
        , _outageEntrydistrict             = fmap Text.pack (Entry._district entry)
        , _outageEntrystreet               = fmap Text.pack (Entry._street entry)
        , _outageEntryradius               = Entry._radius entry
        , _outageEntryCoordinateSystemType = Text.pack (Entry._CoordinateSystemType entry)
        , _outageEntrycoordinates          = fmap Text.pack (Entry._coordinates entry)
        , _outageEntryliveInfo             = fmap Text.pack (Entry._liveInfo entry)
        , _outageEntrycomments             = fmap Text.pack (Entry._comments entry)
        , _outageEntrydefinition           = fmap Text.pack (Entry._definition entry)
        , _outageEntrysocial               = fmap Text.pack (Entry._social entry)
        , _outageEntrysocialText           = fmap Text.pack (Entry._socialText entry)
        , _outageEntryisOutageInArea       = Entry._isOutageInArea entry
        , _outageEntryisFixed              = Entry._isFixed entry
        , _outageEntrySectorType           = fromIntegral (Entry._SectorType entry)
        , _outageEntryoperatorName         = Text.pack (Entry._operatorName entry)
        }

data DB f = DB
  { _dbOutageEntries :: f (TableEntity OutageEntryT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

makeLenses ''DB

db :: DatabaseSettings be DB
db = defaultDbSettings

migrateSqlitePool :: Pool Connection -> IO ()
migrateSqlitePool p = Pool.withResource p migrateSqlite

migrateSqlite :: Connection -> IO ()
migrateSqlite conn = do
  let t :: Migrate.CheckedDatabaseSettings Sqlite DB =
        DB
          { _dbOutageEntries = Migrate.CheckedDatabaseEntity (Migrate.checkedDbEntityAuto "outage_entries") []
          }
  void
    $ runBeamSqlite conn
    $ Migrate.createSchema Sqlite.migrationBackend t

insertBatch :: DatabaseEntity Sqlite db (TableEntity OutageEntryT) -> [OutageEntryT Identity] -> SqlInsert Sqlite OutageEntryT
insertBatch tbl rows =
  insertOnConflict
    tbl
    (insertValues rows)
    anyConflict
    (onConflictUpdateSet $ \fields _oldValue ->
      let u1 = _outageEntrydateEnd fields <-. current_ (_outageEntrydateEnd fields)
          u2 = _outageEntrylastUpdate fields <-. current_ (_outageEntrylastUpdate fields)
          u3 = _outageEntryDurationMinutes fields <-. current_ (_outageEntryDurationMinutes fields)
      in u1 <> u2 <> u3
    )

{-| Import a stream of rows into a sqlite DB
-}
imports ::
  DatabaseEntity Sqlite db (TableEntity OutageEntryT)
  -> Pool Sqlite.Connection
  -> Stream (Of (OutageEntryT Identity)) IO a
  -> IO a
imports table pool stream =
  S.mapM_ (\rows -> Pool.withResource pool $ \conn -> trySqlite (runBeamSqlite conn $ runInsert $ insertBatch table rows))
  $ S.mapped S.toList
  $ S.chunksOf maxRows
  $ stream

trySqlite :: IO () -> IO ()
trySqlite action =
  let go attempts = catch action (handleError attempts)
      handleError n err@Sqlite.SQLError{Sqlite.sqlError=Sqlite.ErrorBusy}
        | n <= 10 = threadDelay 3_000_000 >> go (succ n)
        | otherwise = putStrLn "too many retries" >> throw err
      handleError _ err = throw err
  in go (0 :: Int)

maxRows :: Int
maxRows = 500

newtype ImportArgs =
  ImportArgs
    { sqliteDbFile :: FilePath -- ^ The sqlite database
    }
    deriving stock (Eq, Show)

{-| Download the latest outage info from the API and save it to the
sqlite database.
-}
importEntriesFromAPI :: ImportArgs -> IO ()
importEntriesFromAPI ImportArgs{sqliteDbFile} = do
  pool <- sqlitePool sqliteDbFile
  createDb sqliteDbFile pool
  entries <- responseBody <$> getEntries
  imports (_dbOutageEntries db) pool
    (dropLeftsWithLog $ S.map entryToRow $ S.each entries)
  Pool.withResource pool $ \conn -> runBeamSqlite conn (Beam.runSelectReturningOne totalEntries) >>= \case
    Nothing -> putStrLn "Import succeeded, query failed"
    Just i -> putStrLn $ "Found " <> show i <> " entries"

createDb :: FilePath -> Pool Connection -> IO ()
createDb fp pool = do
  c <- doesFileExist fp
  unless c $ do
    migrateSqlitePool pool

sqlitePool :: String -> IO (Pool Sqlite.Connection)
sqlitePool db' =
  let cfg =
        Pool.PoolConfig
          { Pool.createResource = Sqlite.open db'
          , Pool.freeResource   = Sqlite.close
          , Pool.poolCacheTTL   = 30
          , Pool.poolMaxResources = 2
          }
  in Pool.newPool cfg

dropLeftsWithLog :: (Show e, MonadIO m) => Stream (Of (Either e a)) m r -> Stream (Of a) m r
dropLeftsWithLog = S.mapMaybeM m where
  m (Left e)  = liftIO (print e) >> pure Nothing
  m (Right a) = pure (Just a)

totalEntries :: SqlSelect Sqlite Int32
totalEntries =
  Beam.select
    $ Beam.aggregate_ (\OutageEntryT{_outageEntryId} -> Beam.count_ _outageEntryId)
    $ Beam.all_ (_dbOutageEntries db)
