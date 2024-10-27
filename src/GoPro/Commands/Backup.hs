{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module GoPro.Commands.Backup (runBackup, runStoreMeta, runStoreMeta', runReceiveS3CopyQueue,
                              runLocalBackup, runDownload, runClearMeta, extractMedia, extractOrig) where


import           Amazonka                        (send)
import           Amazonka.Lambda                 (InvocationType (..), newInvoke)
import           Amazonka.S3                     (BucketName (..))
import           Amazonka.SQS                    (newDeleteMessageBatch, newDeleteMessageBatchRequestEntry,
                                                  newReceiveMessage)
import           Cleff                           hiding (send)
import           Cleff.Fail
import           Cleff.Reader                    (Reader)
import           Conduit
import           Control.Applicative             (optional)
import           Control.Lens
import           Control.Monad                   (unless, void)
import           Control.Monad.Trans.Maybe       (MaybeT (..), runMaybeT)
import           Control.Monad.Trans.Writer.Lazy (execWriterT, tell)
import           Control.Retry                   (RetryStatus (..), exponentialBackoff, limitRetries, recoverAll)
import qualified Data.Aeson                      as J
import           Data.Aeson.Lens
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as BL
import           Data.Foldable                   (asum, fold, for_, traverse_)
import           Data.Functor                    (($>))
import           Data.Generics.Labels            ()
import           Data.List                       (nubBy, sort)
import           Data.List.Extra                 (chunksOf)
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromJust, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set                        as Set
import           Data.String                     (fromString)
import           Data.Text                       (Text, isInfixOf, isSuffixOf, pack, stripPrefix, toLower, unpack)
import qualified Data.Text.Encoding              as TE
import           Data.Time.Clock.POSIX           (utcTimeToPOSIXSeconds)
import           Data.Time.Format
import           Network.HTTP.Simple             (getResponseBody, httpSource, parseRequest)
import           Safe.Exact                      (zipWithExactMay)
import qualified Shelly                          as Sh
import           System.Directory                (createDirectoryIfMissing, doesFileExist, renameDirectory, renameFile)
import           System.Directory.PathWalk       (WalkStatus (..), pathWalkInterruptible)
import           System.FilePath.Posix           (takeDirectory, takeExtension, takeFileName, (</>))
import           System.Posix.Files              (createLink, setFileTimes)
import           UnliftIO                        (concurrently, mapConcurrently, mapConcurrently_,
                                                  pooledMapConcurrentlyN_)

import           GoPro.AuthCache
import           GoPro.Commands
import           GoPro.Config.Effect
import           GoPro.DB
import qualified GoPro.File                      as GPF
import           GoPro.Logging
import           GoPro.Plus.Media
import           GoPro.S3

retryRetrieve :: [LogFX, AuthCache, IOE] :>> es => J.FromJSON j => MediumID -> Eff es j
retryRetrieve mid = recoverAll policy $ \r -> do
  unless (rsIterNumber r == 0) $ logInfoL ["retrying metadata ", tshow mid, " attempt ", tshow (rsIterNumber r)]
  retrieve mid
  where policy = exponentialBackoff 2000000 <> limitRetries 9

type LambdaFunc = Text

copyMedia :: [Reader Options, AuthCache, Fail, LogFX, S3, DatabaseEff, IOE] :>> es => LambdaFunc -> Extractor -> MediumID -> Eff es ()
copyMedia λ extract mid = do
  todo <- extract mid <$> retryRetrieve mid
  pooledMapConcurrentlyN_ 5 copy todo
  queuedCopyToS3 (map (\(k,_,_) -> (mid, unpack k)) todo)

  where
    copy (k, h, u) = do
      b <- s3Bucket
      logDbgL ["Queueing copy of ", mid, " to ", tshow k]
      inAWS $ \env -> void . send env $ newInvoke λ (encodeCopyRequest (pack h) (pack u) b k) & #invocationType ?~ InvocationType_Event

    encodeCopyRequest hd src (BucketName bname) k = BL.toStrict . J.encode $ jbod
      where
        dest = J.Object (mempty & at "bucket" ?~ J.String bname
                          & at "key" ?~ J.String k)
        jbod = J.Object (mempty & at "src" ?~ J.String src
                          & at "head" ?~ J.String hd
                          & at "dest" ?~ dest
                          & at "mid" ?~ J.String mid)

downloadLocally :: [Reader Options, AuthCache, LogFX, IOE] :>> es => FilePath -> Extractor -> Medium -> Eff es ()
downloadLocally path extract Medium{..} = do
  logInfoL ["Beginning backup of ", tshow _medium_id]
  vars <- retryRetrieve _medium_id
  refdir <- asksOpt optReferenceDir
  locals <- fromMaybe mempty <$> traverse GPF.fromDirectoryFull refdir
  let todo = extract _medium_id vars
      srcs = maybe [] NE.toList $ Map.lookup (vars ^. filename) locals
  copyLocal todo srcs

  pooledMapConcurrentlyN_ 5 downloadNew todo

  linkNames (filter ("-var-source" `isInfixOf`) . fmap (\(a,_,_) -> a) $ todo)

  -- This is mildly confusing since the path inherently has the mid in the path.
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory midPath)
    renameDirectory (tmpdir </> unpack _medium_id) midPath
    setFileTimes midPath (toEpochTime _medium_captured_at) (toEpochTime _medium_captured_at)
  logInfoL ["Completed backup of ", tshow _medium_id]

  where
    midPath = path </> formatTime defaultTimeLocale "%0Y/%m" _medium_captured_at </> unpack _medium_id
    tmpdir = path </> "tmp"

    tmpFilename k = tmpdir </> (unpack . fromJust . stripPrefix "derivatives/") k

    store dest a = liftIO (doesFileExist dest) >>= \exists ->
      if exists then logDbgL ["Using existing file: ", tshow dest]
      else a tmpfile *> liftIO (renameFile tmpfile dest)

      where
        tmpfile = dest <> ".tmp"

    copyLocal :: [Reader Options, LogFX, IOE] :>> es => [(Text, String, String)] -> [GPF.File] -> Eff es ()
    copyLocal devs refs = do
      let srcdevs = sort $ mapMaybe (\(a,_,_) -> if "-var-source" `isInfixOf` a then Just a else Nothing) devs

      case zipWithExactMay (\a b -> (GPF._gpFilePath b, tmpFilename a)) srcdevs refs of
        Nothing      -> logDbgL ["no match ", tshow ((\(a,_,_) -> a) <$> devs), " ", tshow refs]
        Just aligned -> do
          logInfoL ["Copying local files", tshow aligned]
          void . Sh.shelly $ traverse (\(_,d) -> Sh.mkdir_p (takeDirectory d)) (Set.toList $ Set.fromList aligned)
          traverse_ (\(s,d) -> store d (\t -> liftIO $ asum [createLink s t,
                                                             Sh.shelly (Sh.cp s t)
                                                            ])) aligned

    downloadNew argh@(k, _, _) = store (tmpFilename k) $ download argh

    download (k, _, u) dest = recoverAll policy $ \r -> do
      liftIO $ createDirectoryIfMissing True dir
      logDbgL ["Fetching ", tshow _medium_id, " ", k, " attempt ", tshow (rsIterNumber r)]
      req <- parseRequest u
      liftIO $ runConduitRes (httpSource req getResponseBody .| sinkFile dest)

        where
          dir = takeDirectory dest
          policy = exponentialBackoff 2000000 <> limitRetries 9

    linkNames names = void . runMaybeT $ do
      fn <- MaybeT . pure $ _medium_filename
      gf <- MaybeT . pure $ GPF.parseGPFileName fn
      let links = zip names (iterate GPF.nextFile gf)
      lift $ logDbgL ["Linking ", tshow names, " for ", tshow fn, tshow links]
      for_ links $ \(bp, GPF.File{..}) -> lift $ do
        let existing = tmpFilename bp
            dir = takeDirectory existing
            new = dir </> _gpFilePath
        logDbgL ["  linking ", tshow existing, " -> ", tshow new]
        liftIO . optional $ createLink existing new

    toEpochTime = fromIntegral @Int . floor . utcTimeToPOSIXSeconds


extractMedia :: Extractor
extractMedia mid fi = filter desirable . nubBy (\(_,_,u1) (_,_,u2) -> u1 == u2) $
                                          fold [ ex "var" variations,
                                                 ex2 "sidecar" sidecar_files,
                                                 otherFiles mid fi
                                               ]
  where

    -- Explicitly ignoring concats because they're derived and huge.
    desirable (fn,_,_) = not ("-concat.mp4" `isSuffixOf` fn)

    ex p l = fi ^.. fileStuff . l . folded . to conv . folded
      where
        conv v = maybeToList $ do
          lbl <- v ^? media_label
          typ <- v ^? media_type
          let h = v ^. media_head
              u = v ^. media_url
              inum = maybe "" (\x -> "-" <> show x) (v ^. media_item_number)
          pure (fromString $ mconcat ["derivatives/", unpack mid, "/", unpack mid, "-", p, "-", lbl, inum, ".", typ],
                fromString h,
                u)
    --TODO:  Figure out how to deduplicate this
    ex2 p l = fi ^.. fileStuff . l . folded . to conv . folded
      where
        conv v = maybeToList $ do
          lbl <- v ^? media_label
          typ <- v ^? media_type
          let h = v ^. media_head
              u = v ^. media_url
              inum = maybe "" (\x -> "-" <> show x) (v ^. media_item_number)
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
extractOrig mid = filter desirable . extractMedia mid
  where
    desirable (toLower -> fn,_,_) = ("-source" `isInfixOf` fn) || ("-baked_source" `isInfixOf` fn)
                                    || (".jpg" `isSuffixOf` fn && "-file" `isInfixOf` fn)
                                    || ("raw_photo.gpr" `isInfixOf` fn)

runBackup :: [Reader Options, ConfigFX, AuthCache, Fail, LogFX, S3, DatabaseEff, IOE] :>> es => Extractor -> Eff es ()
runBackup ex = do
  λ <- configItem CfgCopyFunc
  todo <- take 5 <$> listToCopyToS3
  logDbgL ["todo: ", tshow todo]
  c <- asksOpt optUploadConcurrency
  pooledMapConcurrentlyN_ c (copyMedia λ ex) todo

runLocalBackup :: [Reader Options, AuthCache, LogFX, DatabaseEff, IOE] :>> es => Extractor -> NonEmpty FilePath -> Eff es ()
runLocalBackup ex paths = listToCopyLocally >>= maybe (pure ()) (runDownload ex paths) . NE.nonEmpty

runDownload :: [Reader Options, AuthCache, LogFX, DatabaseEff, IOE] :>> es => Extractor -> NonEmpty FilePath -> NonEmpty MediumID -> Eff es ()
runDownload ex paths mids = do
  have <- fold <$> liftIO (traverse findHave paths)
  let todo = filter (`Set.notMember` have) (NE.toList mids)
  logDbgL ["todo: ", tshow todo]
  c <- asksOpt optDownloadConcurrency
  pooledMapConcurrentlyN_ c one todo

  where
    one mid = loadMedium mid >>= \case
      Nothing -> logErrorL ["Cannot find record for ", mid]
      Just m  -> downloadLocally (NE.head paths) ex m

    findHave path = execWriterT . pathWalkInterruptible path $ \dir subdirs _filenames ->
      case takeFileName dir of
        "Proxy" -> pure StopRecursing
        "tmp"   -> pure StopRecursing
        _       -> tell (Set.fromList (pack <$> subdirs)) $> Continue

runStoreMeta :: [Reader Options, AuthCache, Fail, LogFX, S3, DatabaseEff, IOE] :>> es => Eff es ()
runStoreMeta = do
  logDbg "Finding metadata blobs stored in S3 and local database"
  (have, local) <- concurrently (Set.fromList <$> listMetaBlobs) selectMetaBlob
  logDbgL ["local: ", (pack.show.fmap fst) local]
  let todo = filter ((`Set.notMember` have) . fst) local
  unless (null todo) $ logInfoL ["storemeta todo: ", (pack.show.fmap fst) todo]

  c <- asksOpt optUploadConcurrency
  pooledMapConcurrentlyN_ c (\(mid,blob) -> storeMetaBlob mid (BL.fromStrict <$> blob)) todo

runStoreMeta' :: [Reader Options, AuthCache, Fail, LogFX, S3, DatabaseEff, IOE] :>> es => [(MediumID, Maybe ByteString)] -> Eff es ()
runStoreMeta' [] = pure ()
runStoreMeta' local = do
  logDbg "Finding metadata blobs stored in S3 and local database"
  have <- Set.fromList <$> listMetaBlobs
  logDbgL ["local: ", (pack.show.fmap fst) local]
  let todo = filter ((`Set.notMember` have) . fst) local
  unless (null todo) $ logInfoL ["storemeta todo: ", (pack.show.fmap fst) todo]

  c <- asksOpt optUploadConcurrency
  pooledMapConcurrentlyN_ c (\(mid,blob) -> storeMetaBlob mid (BL.fromStrict <$> blob)) todo

runClearMeta :: [Reader Options, Fail, LogFX, S3, DatabaseEff, IOE] :>> es => Eff es ()
runClearMeta = do
  (have, local) <- concurrently (Set.fromList <$> listMetaBlobs) selectMetaBlob
  let backedup =  filter (`Set.member` have) (fst <$> local)
  logDbgL ["clearing ", tshow backedup]
  clearMetaBlob backedup

runReceiveS3CopyQueue :: [Reader Options, ConfigFX, LogFX, DatabaseEff, IOE] :>> es => Eff es ()
runReceiveS3CopyQueue = do
  qrl <- configItem CfgCopySQSQueue
  go qrl =<< listS3Waiting

    where
      go _ [] = logInfo "Not waiting for any results"
      go qrl w = do
        logInfoL ["Waiting for ", tshow (length w), " files to finish copying"]
        msgs <- toListOf (folded . #messages . folded) <$> getmsgs qrl (length w)

        logInfoL ["Processing ", tshow (length msgs), " responses"]
        let results = computeResults <$> msgs
        mapM_ (\(fn, ok, _) -> logDbgL ["Finished ", fn, " with status: ", tshow ok]) results
        markS3CopyComplete results

        let mids = msgs ^.. folded . folded . #receiptHandle . _Just
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
                           & #maxNumberOfMessages ?~ 10
                           & #waitTimeSeconds ?~ (if n == 0 then 20 else 0)
                           & #visibilityTimeout ?~ 60

      delMessages qrl = mapConcurrently_ batch . chunksOf 10
        where batch dels = inAWS $ \env -> void . send env $ newDeleteMessageBatch qrl & #entries .~ dels

      computeResults m = do
        let bodBytes = m ^?! folded . #body . _Just . to (BL.fromStrict . TE.encodeUtf8)
            bod = J.decode @J.Value bodBytes
            modbod = bod & _Just . key "requestPayload" . _Object %~ sans "src" . sans "head"
            condition = bod ^?! _Just . key "requestContext" . key "condition" . _String
            fn = bod ^?! _Just . key "requestPayload" . key "dest" . key "key" . _String
          in (fn, condition == "Success", modbod)
