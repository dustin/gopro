{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DBSpec where

import           Cleff
import           Cleff.Fail
import           Control.Lens                         hiding (elements)
import           Control.Monad                        (forM_)
import qualified Data.Aeson                           as J
import           Data.ByteString                      (ByteString)
import           Data.List                            (sortOn)
import qualified Data.Map.Strict                      as Map
import           Data.Ord                             (Down (..))
import           Data.Time

import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck                as QC

import           GoPro.DEVC                           (Location (..))
import           GoPro.Plus.Arbitrary
import           GoPro.Plus.Auth                      (AuthInfo (..))
import           GoPro.Plus.Media

import           GoPro.DB
import           GoPro.DB.Sqlite                      as SDB
import           GoPro.Resolve
import Data.Maybe (isNothing)

instance Arbitrary MediaRow where
  arbitrary = MediaRow <$> (unTruncated <$>  arbitrary) <*> arbitrary <*> arbitrarymJSON <*> arbitraryJSON
    where
      arbitraryJSON = pure "{}"
      arbitrarymJSON = oneof [pure (Just "{}"), pure Nothing]

deriving instance Eq AuthInfo

prop_authStorage :: NonEmptyList AuthInfo -> Property
prop_authStorage (NonEmpty ais) = ioProperty . runDB $ do
  mapM_ updateAuth ais
  AuthResult loaded _ <- loadAuth
  pure (loaded === last ais)

runDB :: Eff [DB, Fail, IOE] a -> IO a
runDB a = runIOE . runFailIO . runDatabaseSqliteStr ":memory:" $ do
  initTables
  a

unit_configOption :: Assertion
unit_configOption = do
  forM_ [minBound..] $ \x -> assertEqual (show x) ((strOption . optionStr) x) (Just x)
  assertEqual "negative case" (strOption "garbage") Nothing

unit_config :: Assertion
unit_config = runDB $ do
  orig <- loadConfig
  updateConfig (Map.insert CfgBucket "busket" orig)
  updated <- loadConfig
  liftIO $ assertBool "modified" (orig /= updated)
  updateConfig orig
  restored <- loadConfig
  liftIO $ assertEqual "restored" orig restored

-- MediaRow with unique media
newtype MediaRowSet = MediaRowSet [MediaRow]
  deriving newtype (Eq, Show)

newtype TruncatedMedium = TruncatedMedium { unTruncated :: Medium }
  deriving newtype (Show, Eq)

instance Arbitrary TruncatedMedium where
  arbitrary = TruncatedMedium . clean <$> arbitrary
    where clean m = m & medium_captured_at %~ truncUTC & medium_created_at %~ truncUTC

truncUTC :: UTCTime -> UTCTime
truncUTC (UTCTime d t) = UTCTime d (fromIntegral @Int $ truncate t)

instance Arbitrary MediaRowSet where
  arbitrary = MediaRowSet . Map.elems . Map.fromList . fmap (\mr -> (mr ^. row_media . medium_id , mr)) <$> listOf arbitrary

  shrink (MediaRowSet xs) = MediaRowSet <$> shrinkList (const []) xs

prop_storeLoad :: MediaRowSet -> Property
prop_storeLoad (MediaRowSet rows) = ioProperty $ do
  got <- runDB $ storeMedia rows *> loadMediaRows
  pure $ sort (rows & traversed . row_media . medium_token .~ "") === sort got
    where sort = sortOn (_medium_id . _row_media)

prop_storeLoadOne :: MediaRowSet -> Property
prop_storeLoadOne (MediaRowSet rows) = ioProperty . runDB $ do
  storeMedia rows
  forM_ rows $ \MediaRow{_row_media=m@Medium{..}} -> do
    got <- loadMedium _medium_id
    pure (got === Just m)

prop_storeLoad2 :: MediaRowSet -> Property
prop_storeLoad2 (MediaRowSet rows) = ioProperty $ do
  got <- runDB $ storeMedia rows *> loadMedia
  let media = sortOn (Down . _medium_captured_at) $ rows ^.. folded . row_media
  pure $ sort (media & traversed . medium_token .~ "") === sort got
    where sort = sortOn _medium_id

prop_storeLoadIDs :: MediaRowSet -> Property
prop_storeLoadIDs (MediaRowSet rows) = ioProperty $ do
  got <- runDB $ storeMedia rows *> loadMediaIDs
  pure $ (sortOn (Down . _medium_captured_at . _row_media) rows ^.. folded . row_media . medium_id) === got

instance Arbitrary MetadataType where arbitrary = arbitraryBoundedEnum

instance Arbitrary MDSummary where
  arbitrary = MDSummary
              <$> aCamera
              <*> (fmap truncUTC <$> arbitrary)
              <*> gmd
              <*> gmd
              <*> gmd
              <*> gmd
              <*> gmd
              <*> gmd
              <*> (getNonNegative <$> arbitrary)
              <*> gloc
    where
      rnd :: (RealFrac a1, Fractional a2) => a1 -> a2
      rnd d = fromIntegral (round (1000 * d)) / 1000
      gloc :: Gen (Maybe (Location, Float))
      gloc = (fmap . fmap) (rnd @Float) <$> arbitrary
      gmd = fmap (rnd @Double) <$> arbitrary

instance Arbitrary Location where
  arbitrary = elements [Snow, Urban, Indoor, Water, Vegetation, Beach]

prop_metaBlob :: TruncatedMedium -> MetadataType -> ByteString -> Property
prop_metaBlob (TruncatedMedium m@(Medium{_medium_id})) mt bs = ioProperty . runDB $ do
  storeMedia [MediaRow m Nothing Nothing (J.toJSON m)]
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
    nullBlobs :: DB :> es => Eff es Int
    nullBlobs = length . filter isNothing <$> allBlobs
    allBlobs :: DB :> es => Eff es [Maybe ByteString]
    allBlobs = traverse (fmap (maybe Nothing snd) . loadMetaBlob) =<< loadMediaIDs

prop_meta :: TruncatedMedium -> MetadataType -> ByteString -> MDSummary -> Property
prop_meta (TruncatedMedium m@(Medium{_medium_id})) mt bs md = ioProperty . runDB $ do
    storeMedia [MediaRow m Nothing Nothing (J.toJSON m)]
    insertMetaBlob _medium_id mt (Just bs)
    insertMeta _medium_id md
    nmd <- selectMeta
    liftIO $ assertEqual "summary metadata" (Map.singleton _medium_id md) nmd

    nmd' <- loadMeta _medium_id
    liftIO $ assertEqual "summary metadata" (Just md) nmd'
