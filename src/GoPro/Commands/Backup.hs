{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module GoPro.Commands.Backup (runBackup, runStoreMeta, runReceiveS3CopyQueue,
                              runLocalBackup, runClearMeta, extractMedia, extractOrig) where


import           Amazonka              (send)
import           Amazonka.Lambda       (InvocationType (..), newInvoke)
import           Amazonka.S3           (BucketName (..))
import           Amazonka.SQS          (newDeleteMessageBatch, newDeleteMessageBatchRequestEntry, newReceiveMessage)
import           Conduit
import           Control.Applicative   ((<|>))
import           Control.Lens
import           Control.Monad         (unless, void)
import           Control.Monad.Reader  (asks)
import           Control.Retry         (RetryStatus (..), exponentialBackoff, limitRetries, recoverAll)
import qualified Data.Aeson            as J
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy  as BL
import           Data.Foldable         (fold)
import           Data.Generics.Product (field)
import           Data.List.Extra       (chunksOf)
import           Data.Maybe            (fromJust, maybeToList)
import qualified Data.Set              as Set
import           Data.String           (fromString)
import           Data.Text             (Text, pack, stripPrefix, unpack)
import qualified Data.Text.Encoding    as TE
import           Network.HTTP.Simple   (getResponseBody, httpSource, parseRequest)
import           System.Directory      (createDirectoryIfMissing, doesFileExist, listDirectory, renameDirectory,
                                        renameFile)
import           System.FilePath.Posix (takeDirectory, takeExtension, (</>))
import           UnliftIO              (concurrently, mapConcurrently, mapConcurrently_)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media
import           GoPro.S3

retryRetrieve :: J.FromJSON j => MediumID -> GoPro j
retryRetrieve mid = recoverAll policy $ \r -> do
  unless (rsIterNumber r == 0) $ logInfoL ["retrying metadata ", tshow mid, " attempt ", tshow (rsIterNumber r)]
  retrieve mid
  where policy = exponentialBackoff 2000000 <> limitRetries 9

type LambdaFunc = Text

copyMedia :: LambdaFunc -> Extractor -> MediumID -> GoPro ()
copyMedia λ extract mid = do
  todo <- extract mid <$> retryRetrieve mid
  mapConcurrentlyLimited_ 5 copy todo
  queuedCopyToS3 (map (\(k,_,_) -> (mid, unpack k)) todo)

  where
    copy (k, h, u) = do
      b <- s3Bucket
      logDbgL ["Queueing copy of ", mid, " to ", tshow k]
      inAWS $ \env -> void . send env $ newInvoke λ (encodeCopyRequest (pack h) (pack u) b k) & field @"invocationType" ?~ InvocationType_Event

    encodeCopyRequest hd src (BucketName bname) k = BL.toStrict . J.encode $ jbod
      where
        dest = J.Object (mempty & at "bucket" ?~ J.String bname
                          & at "key" ?~ J.String k)
        jbod = J.Object (mempty & at "src" ?~ J.String src
                          & at "head" ?~ J.String hd
                          & at "dest" ?~ dest
                          & at "mid" ?~ J.String mid)

downloadLocally :: FilePath -> Extractor -> MediumID -> GoPro ()
downloadLocally path extract mid = do
  logInfoL ["Beginning backup of ", tshow mid]
  todo <- extract mid <$> retryRetrieve mid
  mapConcurrentlyLimited_ 5 copynew todo
  -- This is mildly confusing since the path inherently has the mid in the path.
  liftIO $ renameDirectory (tmpdir </> unpack mid) midPath
  logInfoL ["Completed backup of ", tshow mid]

  where
    midPath = path </> unpack mid
    tmpdir = path </> "tmp"

    copynew argh@(k, _, _) = liftIO (doesFileExist dest) >>= \exists ->
      if exists then (logDbgL ["Using existing file: ", tshow dest] >> pure dest)
      else copy argh dest

        where
          dest = tmpdir </> (unpack . fromJust . stripPrefix "derivatives/") k

    copy (k, _, u) dest = recoverAll policy $ \r -> do
      liftIO $ createDirectoryIfMissing True dir
      logDbgL ["Fetching ", tshow mid, " ", k, " attempt ", tshow (rsIterNumber r)]
      req <- parseRequest u
      liftIO $ runConduitRes (httpSource req getResponseBody .| sinkFile tmpfile) >>
        renameFile tmpfile dest
      pure dest

        where
          tmpfile = dest <> ".tmp"
          dir = takeDirectory dest
          policy = exponentialBackoff 2000000 <> limitRetries 9

class Numbered c where
  item_num :: Lens' c (Maybe Int)

instance Numbered SidecarFile where item_num = lens (const Nothing) (\x -> const x)

instance Numbered Variation where item_num = media_item_number

extractMedia :: Extractor
extractMedia mid fi = fold [ ex "var" variations,
                             ex "sidecar" sidecar_files,
                             otherFiles mid fi
                           ]
  where

    ex p l = fi ^.. fileStuff . l . folded . to conv . folded
      where
        conv v = maybeToList $ do
          lbl <- v ^? media_label
          typ <- v ^? media_type
          let h = v ^. media_head
              u = v ^. media_url
              inum = maybe "" (\x -> "-" <> show x) (v ^. item_num)
          pure (fromString $ mconcat ["derivatives/", unpack mid, "/", unpack mid, "-", p, "-", lbl, inum, ".", typ],
                fromString h,
                u)

otherFiles :: Extractor
otherFiles mid fi = fi ^.. fileStuff . files . folded . to conv
  where
    typ = fi ^. filename . to (drop 1 . takeExtension)
    conv v = let i = v ^. file_item_number
                 lbl = show i <> "." <> typ
                 h = v ^. media_head
                 u = v ^. media_url
             in (fromString $ mconcat ["derivatives/", unpack mid, "/", unpack mid, "-files-", lbl],
                 fromString h,
                 u)

extractOrig :: Extractor
extractOrig mid fi = maybe (otherFiles mid fi) (:[]) (srcd "concat" <|> srcd "source")
  where
    srcd lbl = fi ^? fileStuff . variations . folded . filtered (has (var_label . only lbl)) . to conv . folded
      where
        conv v = do
          typ <- v ^? media_type
          let h = v ^. media_head
              u = v ^. media_url
          pure (fromString $ mconcat ["derivatives/", unpack mid, "/" , unpack mid, "-var-", lbl, ".", typ],
                fromString h,
                u)

runBackup :: Extractor -> GoPro ()
runBackup ex = do
  λ <- asks (configItem CfgCopyFunc)
  todo <- take 5 <$> listToCopyToS3
  logDbgL ["todo: ", tshow todo]
  c <- asks (optUploadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (copyMedia λ ex) todo

runLocalBackup :: Extractor -> FilePath -> GoPro ()
runLocalBackup ex path = do
  have <- Set.fromList . fmap pack <$> liftIO (listDirectory path)
  todo <- filter (`Set.notMember` have) <$> listToCopyLocally
  logDbgL ["todo: ", tshow todo]
  c <- asks (optDownloadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (downloadLocally path ex) todo

runStoreMeta :: GoPro ()
runStoreMeta = do
  (have, local) <- concurrently (Set.fromList <$> listMetaBlobs) selectMetaBlob
  logDbgL ["local: ", (pack.show.fmap fst) local]
  let todo = filter ((`Set.notMember` have) . fst) local
  unless (null todo) $ logInfoL ["storemeta todo: ", (pack.show.fmap fst) todo]

  c <- asks (optUploadConcurrency . gpOptions)
  mapConcurrentlyLimited_ c (\(mid,blob) -> storeMetaBlob mid (BL.fromStrict <$> blob)) todo

runClearMeta :: GoPro()
runClearMeta = do
  (have, local) <- concurrently (Set.fromList <$> listMetaBlobs) selectMetaBlob
  let backedup =  filter (`Set.member` have) (fst <$> local)
  logDbgL ["clearing ", tshow backedup]
  clearMetaBlob backedup

runReceiveS3CopyQueue :: GoPro ()
runReceiveS3CopyQueue = do
  qrl <- asks (configItem CfgCopySQSQueue)
  go qrl =<< listS3Waiting

    where
      go _ [] = logInfo "Not waiting for any results"
      go qrl w = do
        logInfoL ["Waiting for ", tshow (length w), " files to finish copying"]
        msgs <- toListOf (folded . field @"messages" . folded) <$> getmsgs qrl (length w)

        logInfoL ["Processing ", tshow (length msgs), " responses"]
        let results = computeResults <$> msgs
        mapM_ (\(fn, ok, _) -> logDbgL ["Finished ", fn, " with status: ", tshow ok]) results
        markS3CopyComplete results

        let mids = msgs ^.. folded . folded . field @"receiptHandle" . _Just
            deletes = zipWith (newDeleteMessageBatchRequestEntry . tshow) [1 :: Int ..] mids
        unless (null deletes) $ do
          logDbg "Deleting processed messages from SQS."
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
          someMessages n = inAWS $ \env -> send env $ newReceiveMessage qrl
                           & field @"maxNumberOfMessages" ?~ 10
                           & field @"waitTimeSeconds" ?~ (if n == 0 then 20 else 0)
                           & field @"visibilityTimeout" ?~ 60

      delMessages qrl = mapConcurrently_ batch . chunksOf 10
        where batch dels = inAWS $ \env -> void . send env $ newDeleteMessageBatch qrl & field @"entries" .~ dels

      computeResults m = do
        let Just bodBytes = m ^? folded . field @"body" . _Just . to (BL.fromStrict . TE.encodeUtf8)
            Just bod = J.decode bodBytes :: Maybe J.Value
            modbod = bod & key "requestPayload" . _Object %~ sans "src" . sans "head"
            Just condition = bod ^? key "requestContext" . key "condition" . _String
            Just fn = bod ^? key "requestPayload" . key "dest" . key "key" . _String
          in (fn, condition == "Success", modbod)
