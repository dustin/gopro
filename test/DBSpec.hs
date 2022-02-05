module DBSpec where

import           Control.Lens                         hiding (elements)
import           Control.Monad                        (forM_)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (ReaderT (..))
import qualified Data.Aeson                           as J
import           Data.ByteString                      (ByteString)
import           Data.List                            (sortOn)
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Ord                             (Down (..))
import           Database.SQLite.Simple

import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck                as QC

import           GoPro.DEVC                           (Location (..))
import           GoPro.Plus.Arbitrary                 ()
import           GoPro.Plus.Media

import           GoPro.DB
import           GoPro.Resolve

instance Arbitrary MediaRow where
  arbitrary = MediaRow <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

runDB :: ReaderT Connection IO a -> IO a
runDB a = withConnection ":memory:" $ \db -> do
  initTables db
  withDB db a

unit_configOption :: Assertion
unit_configOption = do
  forM_ [minBound..] $ \x -> assertEqual (show x) ((strOption . optionStr) x) (Just x)
  assertEqual "negative case" (strOption "garbage") Nothing

unit_config :: Assertion
unit_config = runDB $ do
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

prop_storeLoad :: MediaRowSet -> Property
prop_storeLoad (MediaRowSet rows) = ioProperty $ do
  got <- runDB (storeMedia rows *> loadMediaRows)
  pure $ (rows & traversed . row_media . medium_token .~ "") === got

prop_storeLoad2 :: MediaRowSet -> Property
prop_storeLoad2 (MediaRowSet rows) = ioProperty $ do
  got <- runDB (storeMedia rows *> loadMedia)
  let media = sortOn (Down . _medium_captured_at) $ rows ^.. folded . row_media
  pure $ (media & traversed . medium_token .~ "") === got

prop_storeLoadIDs :: MediaRowSet -> Property
prop_storeLoadIDs (MediaRowSet rows) = ioProperty $ do
  got <- runDB (storeMedia rows *> loadMediaIDs)
  pure $  (sortOn (Down . _medium_captured_at . _row_media) rows ^.. folded . row_media . medium_id) === got

instance Arbitrary MetadataType where arbitrary = arbitraryBoundedEnum

instance Arbitrary MDSummary where
  arbitrary = MDSummary
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary Location where
  arbitrary = elements [Snow, Urban, Indoor, Water, Vegetation, Beach]

prop_metaBlob :: Medium -> MetadataType -> ByteString -> Property
prop_metaBlob m@(Medium{_medium_id}) mt bs = ioProperty . runDB $ do
    storeMedia [MediaRow m Nothing "" (J.encode m)]
    todo <- fmap fst <$> metaBlobTODO
    liftIO $ assertEqual "todo" [_medium_id] todo
    insertMetaBlob _medium_id mt (Just bs)
    todo' <- fmap fst <$> metaBlobTODO
    liftIO $ assertEqual "todo after" [] todo'
    [(i,t,b)] <- metaTODO
    liftIO $ do
      assertEqual "md todo id" _medium_id i
      assertEqual "md todo type" mt t
      assertEqual "md todo bytes" bs b
    [(i, Just b)] <- selectMetaBlob
    liftIO $ do
      assertEqual "sel id" _medium_id i
      assertEqual "sel blob" bs b
    nullBlobs >>= \n -> liftIO $ assertEqual "null blobs" 0 n
    clearMetaBlob [_medium_id]
    nullBlobs >>= \n -> liftIO $ assertEqual "null blobs" 1 n

  where
    nullBlobs :: (HasGoProDB m, MonadIO m) => m Int
    nullBlobs = goproDB >>= \db -> fromOnly . head <$> liftIO (query_ db "select count(*) from metablob where meta is null")

prop_meta :: Medium -> MetadataType -> ByteString -> MDSummary -> Property
prop_meta m@(Medium{_medium_id}) mt bs md = ioProperty . runDB $ do
    storeMedia [MediaRow m Nothing "" (J.encode m)]
    insertMetaBlob _medium_id mt (Just bs)
    insertMeta _medium_id md
    nmd <- selectMeta
    liftIO $ assertEqual "summary metadata" (Map.singleton _medium_id md) nmd
