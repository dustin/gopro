import           Control.Lens
import           Control.Monad         (forM_)
import qualified Data.Aeson            as J
import qualified Data.ByteString.Lazy  as BL

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           GoPro.Commands.Backup (extractMedia, extractOrig)
import           GoPro.DB
import           GoPro.Plus.Media

testExtractMedia :: Assertion
testExtractMedia = do
  Just fi <- J.decode <$> BL.readFile "test/mediaex.json" :: IO (Maybe FileInfo)
  assertEqual (show fi) [("derivatives/xxx/xxx-var-timelapse_video.mp4","hhttp://d/","http://d/"),
                         ("derivatives/xxx/xxx-var-high_res_proxy_mp4.mp4","hhttp://e/","http://e/"),
                         ("derivatives/xxx/xxx-var-mp4_low.mp4","hhttp://f/","http://f/"),
                         ("derivatives/xxx/xxx-sidecar-ziplabel.zip","hhttp://b/","http://b/"),
                         ("derivatives/xxx/xxx-files-1.JPG", "hhttp://a/", "http://a/"),
                         ("derivatives/xxx/xxx-files-2.JPG", "hhttp://aprime/", "http://aprime/")
                         ] $
    extractMedia "xxx" fi

testExtractOrig :: Assertion
testExtractOrig = do
  Just fi <- J.decode <$> BL.readFile "test/mediaex.json" :: IO (Maybe FileInfo)
  assertEqual (show fi) [("derivatives/xxx/xxx-files-1.JPG", "hhttp://a/", "http://a/"),
                         ("derivatives/xxx/xxx-files-2.JPG", "hhttp://aprime/", "http://aprime/")
                         ] $
    extractOrig "xxx" fi

  Just fiv <- J.decode <$> BL.readFile "test/mediavid.json" :: IO (Maybe FileInfo)
  assertEqual (show fi) [("derivatives/xxx/xxx-var-source.mp4", "hhttp://k/", "http://k/")
                         ] $
    extractOrig "xxx" fiv


testConfigOption :: Assertion
testConfigOption = do
  forM_ [minBound..] $ \x -> assertEqual (show x) ((strOption . optionStr) x) (Just x)
  assertEqual "negative case" (strOption "garbage") Nothing

tests :: [TestTree]
tests = [
  testCase "extracting all media" testExtractMedia,
  testCase "extracting orig media" testExtractOrig,
  testCase "config options round trip" testConfigOption
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
