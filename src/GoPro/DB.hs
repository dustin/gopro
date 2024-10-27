{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (MediaRow(..), row_fileInfo, row_media, row_thumbnail, row_variants, row_raw_json,
                 Area(..), area_id, area_name, area_nw, area_se,
                 PartialUpload(..),
                 MetadataType(..),
                 ConfigOption(..), strOption, optionStr,
                 AuthResult(..),
                 FileData(..), fd_medium, fd_section, fd_label, fd_type, fd_item_num, fd_file_size,
                 DB(..),
                 initTables, loadConfig, updateConfig, updateAuth, loadAuth,
                 storeMedia, loadMediaIDs, loadMediaRows, loadMedia, loadMedium, loadThumbnail,
                 storeMoments, loadMoments, momentsTODO, metaBlobTODO, insertMetaBlob, loadMetaBlob,
                 selectMetaBlob, clearMetaBlob, metaTODO, insertMeta, selectMeta, loadMeta,
                 storeUpload, completedUploadPart, completedUpload, listPartialUploads, clearUploads,
                 listQueuedFiles, listToCopyToS3, queuedCopyToS3, markS3CopyComplete, listS3Waiting,
                 listToCopyLocally, selectAreas, foldGPSReadings, storeGPSReadings, gPSReadingsTODO,
                 fileTODO, storeFiles, loadFiles, fixupQuery
                 ) where

import           Cleff

import           Control.Foldl          (Fold (..))
import           Control.Lens           hiding (Fold, (.=))
import           Data.Aeson             (FromJSON (..), ToJSON (..), defaultOptions, fieldLabelModifier,
                                         genericToEncoding, (.=))
import qualified Data.Aeson             as J
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Map.Strict        (Map)
import           Data.Text              (Text)
import           Generics.Deriving.Base (Generic)

import           GoPro.Config.Option
import           GoPro.DEVC             (GPSReading (..))
import           GoPro.Plus.Auth        (AuthInfo (..))
import           GoPro.Plus.Media       (FileInfo (..), Medium (..), MediumID, Moment (..))
import           GoPro.Plus.Upload      (DerivativeID, Upload (..), UploadID)
import           GoPro.Resolve          (MDSummary (..))

instance J.FromJSON MediaRow where
  parseJSON o = do
    m <- J.parseJSON o
    pure $ MediaRow m Nothing "" o

data MediaRow = MediaRow
    { _row_media     :: Medium
    , _row_thumbnail :: Maybe BL.ByteString
    , _row_variants  :: J.Value
    , _row_raw_json  :: J.Value
    } deriving (Show, Eq)
makeLenses ''MediaRow

instance ToJSON MediaRow where
  toJSON (MediaRow m _ v r) = J.object [ "medium" .= m , "variants" .= v , "raw" .= r ]

row_fileInfo :: Lens' MediaRow (Maybe FileInfo)
row_fileInfo = lens (\(MediaRow _ _ v _) -> unj v) (\(MediaRow m t _ r) x -> MediaRow m t (J.toJSON x) r)
  where
    unj :: J.Value -> Maybe FileInfo
    unj x = case J.fromJSON x of
                 J.Success fi -> Just fi
                 J.Error _    -> Nothing

data MetadataType = GPMF | EXIF | NoMetadata deriving (Show, Read, Enum, Bounded, Eq)

data Area = Area
    { _area_id   :: Int
    , _area_name :: String
    , _area_nw   :: (Double, Double)
    , _area_se   :: (Double, Double)
    }
    deriving (Generic, Show)

makeLenses ''Area

instance ToJSON Area where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 6}

data PartialUpload = PartialUpload
  { _pu_filename  :: FilePath
  , _pu_medium_id :: MediumID
  , _pu_upid      :: UploadID
  , _pu_did       :: DerivativeID
  , _pu_partnum   :: Integer
  , _pu_chunkSize :: Integer
  , _pu_parts     :: [Integer]
  }

data AuthResult = AuthResult {
  arInfo    :: AuthInfo,
  arExpired :: Bool
  }
  deriving Show

data FileData = FileData {
  _fd_medium    :: MediumID,
  _fd_section   :: Text,
  _fd_label     :: Text,
  _fd_type      :: Text,
  _fd_item_num  :: Int,
  _fd_file_size :: Int
  } deriving (Generic, Show)

makeLenses ''FileData

instance ToJSON FileData where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 4}
  toJSON = J.genericToJSON defaultOptions { fieldLabelModifier = drop 4}

data DB :: Effect where
  InitTables :: DB m ()
  LoadConfig :: DB m (Map ConfigOption Text)
  UpdateConfig :: Map ConfigOption Text -> DB m ()

  UpdateAuth :: AuthInfo -> DB m ()
  LoadAuth :: DB m AuthResult

  StoreMedia :: [MediaRow] -> DB m ()
  LoadMediaIDs :: DB m [MediumID]
  LoadMediaRows :: DB m [MediaRow]
  LoadMedia :: DB m [Medium]
  LoadMedium :: MediumID -> DB m (Maybe Medium)
  LoadThumbnail :: MediumID -> DB m (Maybe BL.ByteString)
  StoreMoments :: MediumID -> [Moment] -> DB m ()
  LoadMoments :: DB m (Map MediumID [Moment])
  MomentsTODO :: DB m [MediumID]
  MetaBlobTODO :: DB m [(MediumID, String)]
  InsertMetaBlob :: MediumID -> MetadataType -> Maybe BS.ByteString -> DB m ()
  LoadMetaBlob :: MediumID -> DB m (Maybe (MetadataType, Maybe BS.ByteString))
  SelectMetaBlob :: DB m [(MediumID, Maybe BS.ByteString)]
  ClearMetaBlob :: [MediumID] -> DB m ()
  MetaTODO :: DB m [(MediumID, MetadataType, BS.ByteString)]
  InsertMeta :: MediumID -> MDSummary -> DB m ()
  SelectMeta :: DB m (Map MediumID MDSummary)
  LoadMeta :: MediumID -> DB m (Maybe MDSummary)
  StoreUpload :: FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> DB m ()
  CompletedUploadPart :: MediumID -> Integer -> Integer -> DB m ()
  CompletedUpload :: MediumID -> Integer -> DB m ()
  ListPartialUploads :: DB m [[PartialUpload]]
  ClearUploads :: DB m ()
  ListQueuedFiles :: DB m [FilePath]
  ListToCopyToS3 :: DB m [MediumID]
  QueuedCopyToS3 :: [(MediumID, String)] -> DB m ()
  MarkS3CopyComplete :: (ToJSON j) => [(Text, Bool, j)] -> DB m ()
  ListS3Waiting :: DB m [String]
  ListToCopyLocally :: DB m [MediumID]
  SelectAreas :: DB m [Area]

  FoldGPSReadings :: MediumID -> Int -> Fold GPSReading b -> DB m b
  StoreGPSReadings :: MediumID -> [GPSReading] -> DB m ()
  GPSReadingsTODO :: DB m [MediumID]

  FileTODO :: DB m [MediumID]
  StoreFiles :: [FileData] -> DB m ()
  LoadFiles :: Maybe MediumID -> DB m [FileData]

  FixupQuery :: Text -> DB m [[(Text, J.Value)]]

makeEffect ''DB
