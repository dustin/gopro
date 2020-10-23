module GoPro.Commands.Backup (runBackup, runStoreMeta, runReceiveS3CopyQueue,
                              extractSources) where


import           Control.Lens
import           Control.Monad           (unless, void)
import           Control.Monad.Reader    (asks)
import           Control.Monad.Trans.AWS (Region (..), send)
import qualified Data.Aeson              as J
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (maybeToList)
import qualified Data.Set                as Set
import           Data.String             (fromString)
import           Data.Text               (Text, pack, unpack)
import qualified Data.Text.Encoding      as TE
import           Network.AWS.Lambda      (InvocationType (..), iInvocationType, invoke)
import           Network.AWS.S3          (BucketName (..))
import           Network.AWS.SQS         (deleteMessageBatch, deleteMessageBatchRequestEntry, dmbEntries, mBody,
                                          mReceiptHandle, receiveMessage, rmMaxNumberOfMessages, rmVisibilityTimeout,
                                          rmWaitTimeSeconds, rmrsMessages)
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
  queuedCopyToS3 (map (\(k,_,_) -> (mid, unpack k)) todo)

  where
    copy (k, h, u) = do
      b <- s3Bucket
      logInfo $ "Queueing copy of " <> mid <> " to " <> tshow k
      inAWS Oregon . void . send $ invoke λ (encodeCopyRequest (pack h) (pack u) b k) & iInvocationType ?~ Event

    encodeCopyRequest hd src (BucketName bname) k = BL.toStrict . J.encode $ jbod
      where
        dest = J.Object (mempty & at "bucket" ?~ J.String bname
                          & at "key" ?~ J.String k)
        jbod = J.Object (mempty & at "src" ?~ J.String src
                          & at "head" ?~ J.String hd
                          & at "dest" ?~ dest
                          & at "mid" ?~ J.String mid)

extractSources :: MediumID -> FileInfo -> [(Text, String, String)]
extractSources mid fi = foldMap (fmap (\(a,b,c) -> (fromString a, fromString b, c)))
                        [ ex "var" variations, ex "sidecar" sidecar_files ]
  where
    ex p l = fi ^.. fileStuff . l . folded . to conv . folded
      where
        conv v = maybeToList $ do
          lbl <- v ^? media_label
          typ <- v ^? media_type
          h <- v ^? media_head
          u <- v ^? media_url
          pure (mconcat ["derivatives/", unpack mid, "/", unpack mid, "-", p, "-", lbl, ".", typ], h, u)

runBackup :: GoPro ()
runBackup = do
  λ <- asks (configItem "s3copyfunc")
  todo <- take 5 <$> listToCopyToS3
  logDbg $ "todo: " <> tshow todo
  c <- asks (optUploadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (\mid -> copyMedia λ mid) todo

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
  qrl <- asks (configItem "s3copySQSQueue")
  go qrl =<< listS3Waiting

    where
      go _ [] = logInfo "Not waiting for any results"
      go qrl w = do
        logInfo $ "Waiting for " <> tshow (length w) <> " files to finish copying"
        msg <- inAWS Oregon . send $ receiveMessage qrl
          & rmMaxNumberOfMessages ?~ 10
          & rmWaitTimeSeconds ?~ 20
          & rmVisibilityTimeout ?~ 60
        mapM_ process (msg ^.. rmrsMessages . folded)
        let mids = msg ^.. rmrsMessages . folded . mReceiptHandle . _Just
            deletes = zipWith (\i -> deleteMessageBatchRequestEntry (tshow i)) [1 :: Int ..] mids
        unless (null deletes) $ inAWS Oregon . void . send $ deleteMessageBatch qrl & dmbEntries .~ deletes
        go qrl =<< listS3Waiting

      process m = do
        let Just bodBytes = m ^? mBody . _Just . to (BL.fromStrict . TE.encodeUtf8)
            Just bod = J.decode bodBytes :: Maybe J.Value
            modbod = bod & key "requestPayload" . _Object %~ sans "src" . sans "head"
            Just condition = bod ^? key "requestContext" . key "condition" . _String
            Just fn = bod ^? key "requestPayload" . key "dest" . key "key" . _String
        logDbg $ "Finished " <> fn <> " with status: " <> condition
        markS3CopyComplete fn (condition == "Success") modbod
