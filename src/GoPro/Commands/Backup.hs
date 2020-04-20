module GoPro.Commands.Backup (runBackup, runStoreMeta) where


import           Control.Lens
import           Control.Monad           (unless, void, when)
import           Control.Monad.Reader    (asks)
import           Control.Monad.Trans.AWS (Region (..), send)
import qualified Data.Aeson              as J
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Set                as Set
import           Data.String             (fromString)
import           Data.Text               (Text, pack, unpack)
import qualified Data.Text.Encoding      as TE
import           Network.AWS.S3          (BucketName (..))
import           Network.AWS.SQS         (sendMessage)
import           UnliftIO                (concurrently)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media
import           GoPro.S3

type QueueURL = Text

storeDerivative :: QueueURL -> MediumID -> String -> GoPro ()
storeDerivative qrl mid d = do
  b <- asks gpBucket
  Just var <- preview (fileStuff . variations . folded . filtered (has (var_label . only d))) <$> retrieve mid
  let u = var ^. var_url
      key = fromString ("derivatives/" <> unpack mid <> "/" <> d <> "." <> (var ^. var_type))
  logInfo $ "Queueing copy of " <>  mid
  inAWS Oregon $ void . send $ sendMessage qrl (encodeCopyRequest (pack u) b key)

      where
        encodeCopyRequest src (BucketName bname) key = TE.decodeUtf8 (BL.toStrict . J.encode $ jbod)
          where
            dest = J.Object (mempty & at "bucket" ?~ J.String bname
                              & at "key" ?~ J.String key)
            jbod = J.Object (mempty & at "src" ?~ J.String src
                              & at "dest" ?~ dest)

runBackup :: GoPro ()
runBackup = do
  args <- asks (optArgv . gpOptions)
  when (length args /= 1) $ fail "A SQS URL must be specified"
  let [qrl] = args
  have <- Set.fromList . fmap fst <$>  allDerivatives
  logDbg $ "have: " <> (pack . show) have
  want <- Set.fromList <$> loadMediaIDs
  let todo = take 25 $  Set.toList (want `Set.difference` have)
  logDbg $ "todo: " <> (pack.show) todo
  c <- asks (optUploadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (\mid -> storeDerivative (pack qrl) mid "source") todo

runStoreMeta :: GoPro ()
runStoreMeta = do
  (have, local) <- concurrently (Set.fromList <$> listMetaBlobs) selectMetaBlob
  logDbg $ "local: " <> (pack.show.fmap fst) local
  let todo = filter ((`Set.notMember` have) . fst) local
  unless (null todo) .logInfo $ "storemeta todo: " <> (pack.show.fmap fst) todo

  c <- asks (optUploadConcurrency . gpOptions)
  _ <- mapConcurrentlyLimited c (\(mid,blob) -> storeMetaBlob mid (BL.fromStrict blob)) todo
  clearMetaBlob (fst <$> local)
