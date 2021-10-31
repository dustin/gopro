module DBSpec (tests) where

import           Control.Lens
import           Control.Monad         (forM_)
import qualified Data.Aeson            as J
import qualified Data.ByteString.Lazy  as BL

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           GoPro.DB

testConfigOption :: Assertion
testConfigOption = do
  forM_ [minBound..] $ \x -> assertEqual (show x) ((strOption . optionStr) x) (Just x)
  assertEqual "negative case" (strOption "garbage") Nothing

tests :: [TestTree]
tests = [
  testCase "config options round trip" testConfigOption
  ]
