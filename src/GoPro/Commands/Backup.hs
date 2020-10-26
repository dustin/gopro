module GoPro.Commands.Backup (runBackup, runStoreMeta, runReceiveS3CopyQueue,
                              runLocalBackup, extractSources) where


import           Conduit
import           Control.Lens
import           Control.Monad           (unless, void)
import           Control.Monad.Reader    (asks)
import           Control.Monad.Trans.AWS (Region (..), send)
import           Control.Retry           (RetryStatus (..), exponentialBackoff, limitRetries, recoverAll)
import qualified Data.Aeson              as J
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy    as BL
import           Data.List.Extra         (chunksOf)
import           Data.Maybe              (fromJust, maybeToList)
import qualified Data.Set                as Set
import           Data.String             (fromString)
import           Data.Text               (Text, pack, stripPrefix, unpack)
import qualified Data.Text.Encoding      as TE
import           Network.AWS.Lambda      (InvocationType (..), iInvocationType, invoke)
import           Network.AWS.S3          (BucketName (..))
import           Network.AWS.SQS         (deleteMessageBatch, deleteMessageBatchRequestEntry, dmbEntries, mBody,
                                          mReceiptHandle, receiveMessage, rmMaxNumberOfMessages, rmVisibilityTimeout,
                                          rmWaitTimeSeconds, rmrsMessages)
import           Network.HTTP.Simple     (getResponseBody, httpSource, parseRequest)
import           System.Directory        (createDirectoryIfMissing, listDirectory, renameDirectory, renameFile)
import           System.FilePath.Posix   (takeDirectory, (</>))
import           UnliftIO                (concurrently, mapConcurrently, mapConcurrently_)

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

downloadLocally :: FilePath -> MediumID -> GoPro ()
downloadLocally path mid = do
    todo <- extractSources mid <$> retrieve mid
    let tmpdir = path </> "tmp"
    mapConcurrentlyLimited_ 5 (copy tmpdir) todo
    liftIO $ renameDirectory tmpdir midPath

  where
    midPath = path </> unpack mid

    copy tmpdir (k, _, u) = recoverAll policy $ \r -> do
      let tmpfile = dest <> ".tmp"
          dir = takeDirectory dest
      liftIO $ createDirectoryIfMissing True dir
      logInfo $ "Fetching " <> tshow mid <> " " <> k <> " attempt " <> tshow (rsIterNumber r)
      req <- parseRequest u
      liftIO $ runConduitRes (httpSource req getResponseBody .| sinkFile tmpfile) >>
        renameFile tmpfile dest
      pure dest

        where
          dest = tmpdir </> (unpack . fromJust . stripPrefix "derivatives/") k
          policy = exponentialBackoff 2000000 <> limitRetries 9

extractSources :: MediumID -> FileInfo -> [(Text, String, String)]
extractSources mid fi = foldMap (fmap (\(a,b,c) -> (fromString a, fromString b, c)))
                        [ ex "var" variations, ex "sidecar" sidecar_files ]
  where
    ex p l = fi ^.. fileStuff . l . folded . to conv . folded
      where
        conv v = maybeToList $ do
          lbl <- v ^? media_label
          typ <- v ^? media_type
          let h = v ^. media_head
              u = v ^. media_url
          pure (mconcat ["derivatives/", unpack mid, "/", unpack mid, "-", p, "-", lbl, ".", typ], h, u)

runBackup :: GoPro ()
runBackup = do
  λ <- asks (configItem "s3copyfunc")
  todo <- take 5 <$> listToCopyToS3
  logDbg $ "todo: " <> tshow todo
  c <- asks (optUploadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (\mid -> copyMedia λ mid) todo

runLocalBackup :: GoPro ()
runLocalBackup = do
  [path] <- asks (optArgv . gpOptions)
  have <- Set.fromList . fmap pack <$> liftIO (listDirectory path)
  want <- Set.fromList <$> listToCopyLocally
  let todo = Set.toList (want `Set.difference` have)
  logDbg $ "todo: " <> tshow todo
  c <- asks (optDownloadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (downloadLocally path) todo

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
        msgs <- toListOf (folded . rmrsMessages . folded) <$> getmsgs qrl (length w)

        logInfo $ "Processing " <> tshow (length msgs) <> " responses"
        let results = computeResults <$> msgs
        mapM_ (\(fn, ok, _) -> logDbg $ "Finished " <> fn <> " with status: " <> tshow ok) results
        markS3CopyComplete results

        let mids = msgs ^.. folded . mReceiptHandle . _Just
            deletes = zipWith (\i -> deleteMessageBatchRequestEntry (tshow i)) [1 :: Int ..] mids
        unless (null deletes) $ do
          logDbg $ "Deleting processed messages from SQS."
          delMessages qrl deletes

        go qrl =<< listS3Waiting

      -- Run a few parallel message getters, allowing for parallel
      -- processing that scales up to our expected result size, with a
      -- maximum of 10 workers (0..9).
      getmsgs qrl waiting = mapConcurrently someMessages [0 .. min 9 (waiting `div` 10)]
        where
          -- We process the queue in parallel, but only long poll on
          -- the first one.  If no messages are available, we don't
          -- want a bunch of workers blocking on the message that may
          -- trickle in, but we want to be able to burst support for
          -- lots of messages.
          someMessages n = inAWS Oregon . send $ receiveMessage qrl
                           & rmMaxNumberOfMessages ?~ 10
                           & rmWaitTimeSeconds ?~ (if n == 0 then 20 else 0)
                           & rmVisibilityTimeout ?~ 60

      delMessages qrl = mapConcurrently_ batch . chunksOf 10
        where batch dels = inAWS Oregon $ void . send $ deleteMessageBatch qrl & dmbEntries .~ dels

      computeResults m = do
        let Just bodBytes = m ^? mBody . _Just . to (BL.fromStrict . TE.encodeUtf8)
            Just bod = J.decode bodBytes :: Maybe J.Value
            modbod = bod & key "requestPayload" . _Object %~ sans "src" . sans "head"
            Just condition = bod ^? key "requestContext" . key "condition" . _String
            Just fn = bod ^? key "requestPayload" . key "dest" . key "key" . _String
          in (fn, condition == "Success", modbod)
