{-# LANGUAGE DeriveGeneric #-}

module GoPro.Resolve where

import           Control.Lens           hiding ((.=))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..), object, (.=))
import           Data.Aeson.Lens
import qualified Data.ByteString        as BS
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import           Data.Time.Clock        (UTCTime (..))
import           Data.Time.LocalTime    (localTimeToUTC, utc)
import           Data.Tuple             (swap)
import           Generics.Deriving.Base (Generic)
import qualified Graphics.HsExif        as E

import           GoPro.DEVC
import           GoPro.GPMF             (parseGPMF)
import           GoPro.Plus.Auth        (HasGoProAuth)
import           GoPro.Plus.Media       (MediumID, retrieve)

data MDSummary = MDSummary
    { _cameraModel  :: String
    , _capturedTime :: Maybe UTCTime
    , _lat          :: Maybe Double
    , _lon          :: Maybe Double
    , _maxSpeed2d   :: Maybe Double
    , _maxSpeed3d   :: Maybe Double
    , _maxFaces     :: Int
    , _mainScene    :: Maybe (Location, Float)
    }
    deriving (Generic, Show)

instance ToJSON MDSummary where
  toJSON MDSummary{..} = object ["camera" .= _cameraModel,
                                  "ts" .= _capturedTime,
                                  "lat" .= _lat,
                                  "lon" .= _lon,
                                  "maxSpeed2d" .= _maxSpeed2d,
                                  "maxSpeed3d" .= _maxSpeed3d,
                                  "maxFaces" .= _maxFaces,
                                  "scene" .= (show . fst <$> _mainScene),
                                  "sceneProb" .= (snd <$> _mainScene)
                                ]

instance FromJSON MDSummary where
  parseJSON _ = pure (MDSummary "" Nothing Nothing Nothing Nothing Nothing 0 Nothing)

parseDEVC :: BS.ByteString -> Either String [DEVC]
parseDEVC = (fmap.fmap) (uncurry mkDEVC) . parseGPMF

summarizeGPMF :: [DEVC] -> MDSummary
summarizeGPMF devc = MDSummary {
  _cameraModel = fromMaybe "" $ devc ^? folded . dev_name,
  _capturedTime = gps ^? folded . gps_time,
  _lat = gps ^? folded . gps_readings . ix 0 . gpsr_lat,
  _lon = gps ^? folded . gps_readings . ix 0 . gpsr_lon,
  _maxSpeed2d = maximumOf (folded . gps_readings . folded .  gpsr_speed2d) gps,
  _maxSpeed3d = maximumOf (folded . gps_readings . folded .  gpsr_speed2d) gps,
  _maxFaces = max 0 $ maximum1Of (folded . dev_telems . folded . tele_values . _TVFaces . to length) devc,
  _mainScene = mainScene
  }

  where mainScene = swap <$> Map.lookupMax avgs
          where ss = devc  ^. folded . dev_telems . folded . tele_values . _TVScene . folded . to Map.assocs
                counts = Map.fromListWith (+) . map (\(a,_) -> (a,1::Float)) $ ss
                sums   = Map.fromListWith (+) ss
                avgs   = Map.fromList $ zipWith (\(k,s) (_,c) -> (s/c,k)) (Map.assocs sums) (Map.assocs counts)

        gps = devc ^.. folded . dev_telems . folded . tele_values . _TVGPS . filtered ((< 500) . view gps_p)

summarizeEXIF :: Map E.ExifTag E.ExifValue -> MDSummary
summarizeEXIF ex = MDSummary {
  _cameraModel = takeWhile (/= '\0') . show $ Map.findWithDefault (E.ExifText "Unknown") E.model ex,
  _capturedTime = localTimeToUTC utc <$> E.getGpsDateTime ex,
  _lat = fst <$> E.getGpsLatitudeLongitude ex,
  _lon = snd <$> E.getGpsLatitudeLongitude ex,
  _maxSpeed2d = Nothing,
  _maxSpeed3d = Nothing,
  _maxFaces = 0,
  _mainScene = Nothing
  }

removeURLs :: Maybe Value -> Maybe Value
removeURLs v = foldr ($) v mods
  where
    mods = [clear "url" "", clear "head" "", clear "urls" (Array mempty), clear "heads" (Array mempty)]
    clear s n = _Just . deep values . _Object . at s ?~ n

fetchVariantsSansURLs :: (HasGoProAuth m, MonadIO m) => MediumID -> m (Maybe Value)
fetchVariantsSansURLs = fmap removeURLs . retrieve
