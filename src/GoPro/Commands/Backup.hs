module GoPro.Commands.Backup (runBackup, runStoreMeta, runReceiveS3CopyQueue) where


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
import qualified Data.Text.Encoding      as TE
import           Network.AWS.Lambda      (InvocationType (..), iInvocationType, invoke)
import           Network.AWS.S3          (BucketName (..))
import           Network.AWS.SQS         (deleteMessage, mBody, mReceiptHandle, receiveMessage, rmMaxNumberOfMessages,
                                          rmVisibilityTimeout, rmWaitTimeSeconds, rmrsMessages)
import           UnliftIO                (concurrently)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media
import           GoPro.S3

type LambdaFunc = Text

copyMedia :: LambdaFunc -> MediumID -> GoPro ()
copyMedia λ mid = do
  todo <- extractSources mid <$> retrieve mid
  mapConcurrentlyLimited_ 5 copy todo
  queuedCopyToS3 (map (\(k,_) -> (mid, unpack k)) todo)

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
    thepair lbl typ url = ("derivatives/" <> unpack mid <> "/" <> unpack mid <> "-" <> lbl <> "." <> typ, url)
    vars = fi ^.. fileStuff . variations . folded . to fromVariation . folded
      where fromVariation v = maybeToList $ do
              lbl <- v ^? var_label
              typ <- v ^? var_type
              url <- v ^? var_url
              pure $ thepair ("var-" <> lbl) typ url
    sidecars = fi ^.. fileStuff . sidecar_files . folded . to fromSidecar . folded
      where fromSidecar obj = maybeToList $ do
              lbl <- obj ^? atKey "label"
              typ <- obj ^? atKey "type"
              url <- obj ^? atKey "url"
              pure $ thepair ("sidecar-" <> lbl) typ url

                where atKey k = key k . _String . to unpack

runBackup :: GoPro ()
runBackup = do
  args <- asks (optArgv . gpOptions)
  when (length args /= 1) $ fail "A Lambda function name must be specified"
  let [λ] = args
  todo <- take 5 <$> listToCopyToS3
  logDbg $ "todo: " <> tshow todo
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

runReceiveS3CopyQueue :: GoPro ()
runReceiveS3CopyQueue = do
  args <- asks (optArgv . gpOptions)
  when (length args /= 1) $ fail "SQS queue must be provided"
  let [qrl] = args
  msg <- inAWS Oregon . send $ receiveMessage (pack qrl)
         & rmMaxNumberOfMessages ?~ 10
         & rmWaitTimeSeconds ?~ 5
         & rmVisibilityTimeout ?~ 10
  mapM_ (process (pack qrl)) (msg ^.. rmrsMessages . folded)

    where
      process q m = do
        let Just bodBytes = m ^? mBody . _Just . to (BL.fromStrict . TE.encodeUtf8)
            Just bod = J.decode bodBytes :: Maybe J.Value
            modbod = bod & key "requestPayload" . _Object %~ sans "src"
            condition = bod ^? key "requestContext" . key "condition" . _String
            Just fn = bod ^? key "requestPayload" . key "dest" . key "key" . _String
            Just rh = m ^? mReceiptHandle . _Just
        logInfo (tshow modbod)
        markS3CopyComplete fn (condition == Just "Success") modbod
        inAWS Oregon . void . send $ deleteMessage q rh
