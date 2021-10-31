module DBSpec (tests) where

import           Control.Lens
import           Control.Monad                        (forM_)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (ReaderT (..))
import           Data.List                            (sortOn)
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Ord                             (Down (..))
import           Database.SQLite.Simple

import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck                as QC

import           GoPro.Plus.Arbitrary                 ()
import           GoPro.Plus.Media

import           GoPro.DB

instance Arbitrary MediaRow where
  arbitrary = MediaRow <$> arbitrary <*> arbitrary <*> arbitrary

runDB :: ReaderT Connection IO a -> IO a
runDB a = withConnection ":memory:" $ \db -> do
  initTables db
  withDB db a

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

-- MediaRow with unique media
newtype MediaRowSet = MediaRowSet [MediaRow] deriving (Eq, Show)

instance Arbitrary MediaRowSet where
  arbitrary = MediaRowSet . Map.elems . Map.fromList . fmap (\mr -> (mr ^. row_media . medium_id , mr))
              <$> listOf arbitrary

storeLoadProperty :: MediaRowSet -> Property
storeLoadProperty (MediaRowSet rows) = ioProperty $ do
  got <- runDB (storeMedia rows *> loadMediaRows)
  pure $ (rows & traversed . row_media . medium_token .~ "") === got

storeLoad2Property :: MediaRowSet -> Property
storeLoad2Property (MediaRowSet rows) = ioProperty $ do
  got <- runDB (storeMedia rows *> loadMedia)
  let media = sortOn (Down . _medium_captured_at) $ rows ^.. folded . row_media
  pure $ (media & traversed . medium_token .~ "") === got

storeLoadIDsProperty :: MediaRowSet -> Property
storeLoadIDsProperty (MediaRowSet rows) = ioProperty $ do
  got <- runDB (storeMedia rows *> loadMediaIDs)
  pure $  (sortOn (Down . _medium_captured_at . _row_media) rows ^.. folded . row_media . medium_id) === got

tests :: [TestTree]
tests = [
  testCase "config options round trip" testConfigOption,
  testCase "config tests" testConfig,
  testProperty "store/load" storeLoadProperty,
  testProperty "store/load2" storeLoad2Property,
  testProperty "store/load IDs" storeLoadIDsProperty
  ]
