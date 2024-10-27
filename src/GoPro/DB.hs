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
                 DatabaseEff(..),
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

data DatabaseEff :: Effect where
  InitTables :: DatabaseEff m ()
  LoadConfig :: DatabaseEff m (Map ConfigOption Text)
  UpdateConfig :: Map ConfigOption Text -> DatabaseEff m ()

  UpdateAuth :: AuthInfo -> DatabaseEff m ()
  LoadAuth :: DatabaseEff m AuthResult

  StoreMedia :: [MediaRow] -> DatabaseEff m ()
  LoadMediaIDs :: DatabaseEff m [MediumID]
  LoadMediaRows :: DatabaseEff m [MediaRow]
  LoadMedia :: DatabaseEff m [Medium]
  LoadMedium :: MediumID -> DatabaseEff m (Maybe Medium)
  LoadThumbnail :: MediumID -> DatabaseEff m (Maybe BL.ByteString)
  StoreMoments :: MediumID -> [Moment] -> DatabaseEff m ()
  LoadMoments :: DatabaseEff m (Map MediumID [Moment])
  MomentsTODO :: DatabaseEff m [MediumID]
  MetaBlobTODO :: DatabaseEff m [(MediumID, String)]
  InsertMetaBlob :: MediumID -> MetadataType -> Maybe BS.ByteString -> DatabaseEff m ()
  LoadMetaBlob :: MediumID -> DatabaseEff m (Maybe (MetadataType, Maybe BS.ByteString))
  SelectMetaBlob :: DatabaseEff m [(MediumID, Maybe BS.ByteString)]
  ClearMetaBlob :: [MediumID] -> DatabaseEff m ()
  MetaTODO :: DatabaseEff m [(MediumID, MetadataType, BS.ByteString)]
  InsertMeta :: MediumID -> MDSummary -> DatabaseEff m ()
  SelectMeta :: DatabaseEff m (Map MediumID MDSummary)
  LoadMeta :: MediumID -> DatabaseEff m (Maybe MDSummary)
  StoreUpload :: FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> DatabaseEff m ()
  CompletedUploadPart :: MediumID -> Integer -> Integer -> DatabaseEff m ()
  CompletedUpload :: MediumID -> Integer -> DatabaseEff m ()
  ListPartialUploads :: DatabaseEff m [[PartialUpload]]
  ClearUploads :: DatabaseEff m ()
  ListQueuedFiles :: DatabaseEff m [FilePath]
  ListToCopyToS3 :: DatabaseEff m [MediumID]
  QueuedCopyToS3 :: [(MediumID, String)] -> DatabaseEff m ()
  MarkS3CopyComplete :: (ToJSON j) => [(Text, Bool, j)] -> DatabaseEff m ()
  ListS3Waiting :: DatabaseEff m [String]
  ListToCopyLocally :: DatabaseEff m [MediumID]
  SelectAreas :: DatabaseEff m [Area]

  FoldGPSReadings :: MediumID -> Int -> Fold GPSReading b -> DatabaseEff m b
  StoreGPSReadings :: MediumID -> [GPSReading] -> DatabaseEff m ()
  GPSReadingsTODO :: DatabaseEff m [MediumID]

  FileTODO :: DatabaseEff m [MediumID]
  StoreFiles :: [FileData] -> DatabaseEff m ()
  LoadFiles :: Maybe MediumID -> DatabaseEff m [FileData]

  FixupQuery :: Text -> DatabaseEff m [[(Text, J.Value)]]

makeEffect ''DatabaseEff
