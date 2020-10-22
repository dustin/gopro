module GoPro.Commands.Backup (runBackup, runStoreMeta) where


import           Control.Lens
import           Control.Monad           (unless, void, when)
import           Control.Monad.Reader    (asks)
import           Control.Monad.Trans.AWS (Region (..), send)
import qualified Data.Aeson              as J
import           Data.Aeson.Lens
import           Data.Bifunctor          (first)
import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (maybeToList)
import qualified Data.Set                as Set
import           Data.String             (fromString)
import           Data.Text               (Text, pack, unpack)
import           Network.AWS.Lambda      (InvocationType (..), iInvocationType, invoke)
import           Network.AWS.S3          (BucketName (..))
import           UnliftIO                (concurrently)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media
import           GoPro.S3

type LambdaFunc = Text

copyMedia :: LambdaFunc -> MediumID -> GoPro ()
copyMedia λ mid = mapM_ copy =<< (extractSources mid <$> retrieve mid)

  where
    copy (k, u) = do
      b <- s3Bucket
      logInfo $ "Queueing copy of " <> mid <> " to " <> tshow k
      inAWS Oregon . void . send $ invoke λ (encodeCopyRequest (pack u) b k) & iInvocationType ?~ Event

    encodeCopyRequest src (BucketName bname) k = BL.toStrict . J.encode $ jbod
      where
        dest = J.Object (mempty & at "bucket" ?~ J.String bname
                          & at "key" ?~ J.String k)
        jbod = J.Object (mempty & at "src" ?~ J.String src
                          & at "dest" ?~ dest
                          & at "mid" ?~ J.String mid)

extractSources :: MediumID -> FileInfo -> [(Text, String)]
extractSources mid fi = foldMap (fmap (first fromString)) [ vars, sidecars ]
  where
    vars = fi ^.. fileStuff . variations . folded . to fromVariation . folded
      where fromVariation v = maybeToList $ do
              lbl <- v ^? var_label
              typ <- v ^? var_type
              url <- v ^? var_url
              pure ("derivatives/" <> unpack mid <> "/" <> unpack mid <> "-" <> lbl <> "." <> typ, url)
    sidecars = fi ^.. fileStuff . sidecar_files . folded . to fromSidecar . folded
      where fromSidecar obj = maybeToList $ do
              lbl <- obj ^? key "label" . _String . to unpack
              typ <- obj ^? key "type" . _String . to unpack
              url <- obj ^? key "url" . _String . to unpack
              pure ("derivatives/" <> unpack mid <> "/" <> unpack mid <> "-sidecar-" <> lbl <> "." <> typ,
                    url)

runBackup :: GoPro ()
runBackup = do
  args <- asks (optArgv . gpOptions)
  when (length args /= 1) $ fail "A Lambda function name must be specified"
  let [λ] = args
  have <- Set.fromList . fmap fst <$>  allDerivatives
  logDbg $ "have: " <> (pack . show) have
  want <- Set.fromList <$> loadMediaIDs
  let todo = take 2 $  Set.toList (want `Set.difference` have)
  logDbg $ "todo: " <> (pack.show) todo
  c <- asks (optUploadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (\mid -> copyMedia (pack λ) mid) todo

runStoreMeta :: GoPro ()
runStoreMeta = do
  (have, local) <- concurrently (Set.fromList <$> listMetaBlobs) selectMetaBlob
  logDbg $ "local: " <> (pack.show.fmap fst) local
  let todo = filter ((`Set.notMember` have) . fst) local
  unless (null todo) .logInfo $ "storemeta todo: " <> (pack.show.fmap fst) todo

  c <- asks (optUploadConcurrency . gpOptions)
  _ <- mapConcurrentlyLimited c (\(mid,blob) -> storeMetaBlob mid (BL.fromStrict blob)) todo
  clearMetaBlob (fst <$> local)
