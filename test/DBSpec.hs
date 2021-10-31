module DBSpec (tests) where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ReaderT (..), runReaderT)
import qualified Data.Map.Strict        as Map
import           Database.SQLite.Simple

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck  as QC

import           GoPro.DB

runDB :: ReaderT Connection IO () -> IO ()
runDB a = withConnection ":memory:" $ \db -> do
  initTables db
  runReaderT a db

testConfigOption :: Assertion
testConfigOption = do
  forM_ [minBound..] $ \x -> assertEqual (show x) ((strOption . optionStr) x) (Just x)
  assertEqual "negative case" (strOption "garbage") Nothing

testConfig :: Assertion
testConfig = runDB $ do
  orig <- goproDB >>= liftIO . loadConfig
  updateConfig (Map.insert CfgBucket "busket" orig)
  updated <- goproDB >>= liftIO . loadConfig
  liftIO $ assertBool "modified" (orig /= updated)
  updateConfig orig
  restored <- goproDB >>= liftIO . loadConfig
  liftIO $ assertEqual "restored" orig restored

tests :: [TestTree]
tests = [
  testCase "config options round trip" testConfigOption,
  testCase "config tests" testConfig
  ]
