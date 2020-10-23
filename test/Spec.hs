import           Control.Lens
import qualified Data.Aeson            as J
import qualified Data.ByteString.Lazy  as BL

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import GoPro.Plus.Media
import GoPro.Commands.Backup (extractSources)

testExtractSources :: Assertion
testExtractSources = do
  Just fi <- J.decode <$> BL.readFile "test/mediaex.json" :: IO (Maybe FileInfo)
  assertEqual (show fi) [("derivatives/xxx/xxx-var-timelapse_video.mp4","http://d/"),
                         ("derivatives/xxx/xxx-var-high_res_proxy_mp4.mp4","http://e/"),
                         ("derivatives/xxx/xxx-var-mp4_low.mp4","http://f/"),
                         ("derivatives/xxx/xxx-sidecar-ziplabel.zip","http://b/")] $ extractSources "xxx" fi

tests :: [TestTree]
tests = [
  testCase "extracting sources" testExtractSources
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
