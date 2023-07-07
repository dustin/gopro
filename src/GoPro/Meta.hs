{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StrictData         #-}
module GoPro.Meta where

import           Control.Lens                      (folded, toListOf)
import           Data.Aeson
import qualified Data.ByteString                   as BS
import           Data.Maybe                        (mapMaybe)
import           Data.Monoid                       (First (..), Last (..), Sum (..))
import           Data.Semigroup                    (Max (..))
import           Geodetics.Geodetic                (Geodetic (..), WGS84 (..), groundDistance, readGroundPosition)
import           GoPro.DEVC                        (GPSReading (..))
import qualified GoPro.DEVC                        as GPMF
import qualified GoPro.GPMF                        as GPMF
import qualified Numeric.Units.Dimensional         as D
import           Numeric.Units.Dimensional.SIUnits

extractReadings :: BS.ByteString -> Either String [GPSReading]
extractReadings = fmap (extractFromDEVC . mapMaybe (uncurry GPMF.mkDEVC)) . GPMF.parseGPMF

extractFromDEVC :: [GPMF.DEVC] -> [GPSReading]
extractFromDEVC = toListOf (folded . GPMF.gpsReadings . folded)

data GPSSummary = GPSSummary {
  _gps_home             :: First (Geodetic WGS84)
  , _gps_last           :: Last (Geodetic WGS84)
  , _gps_max_distance   :: Max Double
  , _gps_max_speed      :: Max Double
  , _gps_max_speed3d    :: Max Double
  , _gps_total_distance :: Sum Double
  }
  deriving stock Show

instance Semigroup GPSSummary where
  a <> b = GPSSummary
             (_gps_home a <> _gps_home b)
             (_gps_last a <> _gps_last b)
             (_gps_max_distance a <> _gps_max_distance b)
             (_gps_max_speed a <> _gps_max_speed b)
             (_gps_max_speed3d a <> _gps_max_speed3d b)
             (_gps_total_distance a <> _gps_total_distance b)

instance Monoid GPSSummary where
  mempty = GPSSummary (First Nothing) (Last Nothing) 0 0 0 0

instance ToJSON GPSSummary where
  toJSON GPSSummary{..} = object ([
                                  "total_distance" .= getSum _gps_total_distance
                                  , "max_distance" .= getMax _gps_max_distance
                                  , "max_speed" .= getMax _gps_max_speed
                                  , "max_speed3d" .= getMax _gps_max_speed3d
                                  ]
                                  <> geob "home" (getFirst _gps_home)
                                  <> geob "last" (getLast _gps_last))
    where
      geob _ Nothing                       = []
      geob k (Just (Geodetic lat lon _ _)) = [k .= [lat D./~ degree, lon D./~ degree]]

summarizeGPS :: [GPSReading] -> GPSSummary
summarizeGPS = foldr f mempty . filter (\GPSReading{..} -> _gpsr_dop < 200)
  where
    f GPSReading{..} o = o <> GPSSummary
                                (First parsed) (Last parsed) (Max (dist hprev))
                                (Max _gpsr_speed2d) (Max _gpsr_speed3d) (Sum (dist mprev))
      where
        parsed = readGroundPosition WGS84 (show _gpsr_lat <> " " <> show _gpsr_lon)
        mprev = getLast (_gps_last o)
        hprev = getFirst (_gps_home o)
        dist mp = case (mp, parsed) of
                 (Just p, Just c) -> maybe 0 (\(a,_,_) -> a D./~ meter) (groundDistance p c)
                 _                -> 0
