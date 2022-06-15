{-# LANGUAGE DeriveGeneric #-}
module GoPro.Meta where

import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString                   as BS
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (mapMaybe)
import           Data.Time.Clock
import           Geodetics.Geodetic                (WGS84 (..), groundDistance, readGroundPosition)
import           GHC.Generics
import qualified GoPro.DEVC                        as GPMF
import qualified GoPro.GPMF                        as GPMF
import qualified Numeric.Units.Dimensional         as D
import           Numeric.Units.Dimensional.SIUnits


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
                                 _gps_lon=GPMF._gpsr_lon r,
                                 _gps_alt=GPMF._gpsr_alt r,
                                 _gps_speed2d=GPMF._gpsr_speed2d r,
                                 _gps_speed3d=GPMF._gpsr_speed3d r,
                                 _gps_precision=GPMF._gps_p g
                                 }) readings periods
      where
        readings = GPMF._gps_readings g
        periods = realToFrac <$> [0, 1 / realToFrac (length readings) .. 1 :: Double]
    someTel _ = []

data GPSSummary = GPSSummary {
  _gps_total_distance :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON GPSSummary where
  toJSON = genericToJSON defaultOptions{ fieldLabelModifier = dropWhile (== '_') }

summarizeGPS :: [GPSReading] -> GPSSummary
summarizeGPS rs = GPSSummary $ evalState f Nothing
  where
    f = foldM (\o GPSReading{..} -> do
                  mprev <- get
                  let parsed = readGroundPosition WGS84 (show _gps_lat <> " " <> show _gps_lon)
                  put parsed
                  pure $ case (mprev, parsed) of
                    (Just prev, Just p) -> o + maybe 0 (\(a,_,_) -> a D./~ meter) (groundDistance prev p)
                    _                   -> o
                 ) 0 $ filter (\GPSReading{..} -> _gps_precision < 200) rs
