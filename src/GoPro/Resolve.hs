{-# LANGUAGE DeriveGeneric #-}

module GoPro.Resolve where

import           Control.Lens                      hiding ((.=))
import           Control.Monad.IO.Class            (MonadIO (..))
import           Data.Aeson                        (ToJSON (..), Value (..), object, (.=))
import           Data.Aeson.Lens
import qualified Data.ByteString                   as BS
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe, mapMaybe)
import           Data.Monoid                       (First (..), Sum (..))
import           Data.Semigroup                    (Max (..))
import           Data.Time.Clock                   (UTCTime (..))
import           Data.Time.LocalTime               (localTimeToUTC, utc)
import           Data.Tuple                        (swap)
import           Generics.Deriving.Base            (Generic)
import qualified Graphics.HsExif                   as E

import           Geodetics.Geodetic                (Geodetic (..))
import           GoPro.DEVC
import           GoPro.GPMF                        (parseGPMF)
import           GoPro.Meta
import           GoPro.Plus.Auth                   (HasGoProAuth)
import           GoPro.Plus.Media                  (MediumID, retrieve)

import qualified Numeric.Units.Dimensional         as D
import           Numeric.Units.Dimensional.SIUnits

data MDSummary = MDSummary
    { _cameraModel  :: String
    , _capturedTime :: Maybe UTCTime
    , _lat          :: Maybe Double
    , _lon          :: Maybe Double
    , _maxSpeed2d   :: Maybe Double
    , _maxSpeed3d   :: Maybe Double
    , _maxDistance  :: Maybe Double
    , _totDistance  :: Maybe Double
    , _maxFaces     :: Int
    , _mainScene    :: Maybe (Location, Float)
    }
    deriving (Generic, Show, Eq)

instance ToJSON MDSummary where
  toJSON MDSummary{..} = object ["camera" .= _cameraModel,
                                  "ts" .= _capturedTime,
                                  "lat" .= _lat,
                                  "lon" .= _lon,
                                  "maxSpeed2d" .= _maxSpeed2d,
                                  "maxSpeed3d" .= _maxSpeed3d,
                                  "maxFaces" .= _maxFaces,
                                  "scene" .= (show . fst <$> _mainScene),
                                  "sceneProb" .= (snd <$> _mainScene),
                                  "maxDistance" .= _maxDistance,
                                  "totalDistance" .= _totDistance
                                ]

parseDEVC :: BS.ByteString -> Either String [DEVC]
parseDEVC = (fmap . mapMaybe) (uncurry mkDEVC) . parseGPMF

summarizeGPMF :: [DEVC] -> MDSummary
summarizeGPMF devc = MDSummary {
  _cameraModel = fromMaybe "" $ devc ^? folded . dev_name,
  _capturedTime = gps ^? folded . gpsr_time,
  _maxSpeed2d = Just $ getMax _gps_max_speed,
  _maxSpeed3d = Just $ getMax _gps_max_speed3d,
  _maxFaces = max 0 $ maximum1Of (folded . dev_telems . folded . tele_values . _TVFaces . to length) devc,
  _mainScene = mainScene,
  _lat = unmAngle latitude <$> getFirst _gps_home,
  _lon = unmAngle longitude <$> getFirst _gps_home,
  _maxDistance = Just $ getMax _gps_max_distance,
  _totDistance = Just $ getSum _gps_total_distance
  }

  where mainScene = swap <$> Map.lookupMax avgs
          where ss = devc  ^. folded . dev_telems . folded . tele_values . _TVScene . folded . to Map.assocs
                sc     = Map.fromListWith (<>) $ (\(a,x) -> (a, (Sum x, Sum 1))) <$> ss
                avgs   = Map.foldMapWithKey (\k (Sum s, Sum c) -> Map.singleton (s/c) k) sc

        gps = devc ^.. folded . gpsReadings . folded . filtered ((< 50) . view gpsr_dop)
        GPSSummary{..} = summarizeGPS $ extractFromDEVC devc

        unmAngle f g = f g D./~ degree

summarizeEXIF :: Map E.ExifTag E.ExifValue -> MDSummary
summarizeEXIF ex = MDSummary {
  _cameraModel = takeWhile (/= '\0') . show $ Map.findWithDefault (E.ExifText "Unknown") E.model ex,
  _capturedTime = localTimeToUTC utc <$> E.getGpsDateTime ex,
  _lat = fst <$> E.getGpsLatitudeLongitude ex,
  _lon = snd <$> E.getGpsLatitudeLongitude ex,
  _maxSpeed2d = Nothing,
  _maxSpeed3d = Nothing,
  _maxDistance = Nothing,
  _totDistance = Nothing,
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
