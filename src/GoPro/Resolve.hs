module GoPro.Resolve where

import           Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Time.Clock (UTCTime (..))
import           Data.Tuple      (swap)

import           GoPro.DEVC
import           GoPro.GPMF      (parseGPMF)

data MDSummary = MDSummary {
  cameraModel  :: String,
  capturedTime :: Maybe UTCTime,
  lat          :: Maybe Double,
  lon          :: Maybe Double,
  maxSpeed2d   :: Maybe Double,
  maxSpeed3d   :: Maybe Double,
  maxFaces     :: Int,
  mainScene    :: Maybe (Location, Float)
  } deriving Show

parseDEVC :: BS.ByteString -> Either String [DEVC]
parseDEVC = (fmap.fmap) (uncurry mkDEVC) . parseGPMF

summarize :: [DEVC] -> MDSummary
summarize devc = MDSummary {
  cameraModel = fromMaybe "" $ devc ^? folded . dev_name,
  capturedTime = gps ^? folded . gps_time,
  lat = gps ^? folded . gps_readings . ix 0 . gpsr_lat,
  lon = gps ^? folded . gps_readings . ix 0 . gpsr_lon,
  maxSpeed2d = maximumOf (folded . gps_readings . folded .  gpsr_speed2d) gps,
  maxSpeed3d = maximumOf (folded . gps_readings . folded .  gpsr_speed2d) gps,
  maxFaces = max 0 $ maximum1Of (folded . dev_telems . folded . tele_values . _TVFaces . to length) devc,
  mainScene = mainScene
  }

  where mainScene = swap <$> Map.lookupMax avgs
          where ss = devc  ^. folded . dev_telems . folded . tele_values . _TVScene . folded . to Map.assocs
                counts = Map.fromListWith (+) . map (\(a,_) -> (a,1::Float)) $ ss
                sums   = Map.fromListWith (+) ss
                avgs   = Map.fromList $ zipWith (\(k,s) (_,c) -> (s/c,k)) (Map.assocs sums) (Map.assocs counts)

        gps = devc ^.. folded . dev_telems . folded . tele_values . _TVGPS . filtered ((< 500) . view gps_p)
