module GoPro.Meta where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Time.Clock
import qualified GoPro.DEVC      as GPMF
import qualified GoPro.GPMF      as GPMF

data GPSReading = GPSReading {
  _gps_time        :: UTCTime
  , _gps_lat       :: Double
  , _gps_lon       :: Double
  , _gps_alt       :: Double
  , _gps_speed2d   :: Double
  , _gps_speed3d   :: Double
  , _gps_precision :: Int
  } deriving (Eq, Show)

extractReadings :: BS.ByteString -> Either String [GPSReading]
extractReadings = fmap extract . GPMF.parseGPMF
  where
    extract = foldMap expand . mapMaybe (uncurry GPMF.mkDEVC)
    expand devc = foldMap (someTel . GPMF._tele_values) (Map.elems $ GPMF._dev_telems devc)
    someTel (GPMF.TVGPS g) = zipWith (\r n ->
                               GPSReading{
                                 _gps_time=addUTCTime n (GPMF._gps_time g),
                                 _gps_lat=GPMF._gpsr_lat r,
                                 _gps_lon=GPMF._gpsr_lat r,
                                 _gps_alt=GPMF._gpsr_alt r,
                                 _gps_speed2d=GPMF._gpsr_speed2d r,
                                 _gps_speed3d=GPMF._gpsr_speed3d r,
                                 _gps_precision=GPMF._gps_p g
                                 }) readings periods
      where
        readings = GPMF._gps_readings g
        periods = realToFrac <$> [0, 1 / realToFrac (length readings) .. 1 :: Double]
    someTel _ = []
