{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Upload (
  runCreateUploads, runCreateMultipart, runResumeUpload
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import qualified Data.Text              as T
import           System.Posix.Files     (fileSize, getFileStatus)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media       (MediumID)
import           GoPro.Plus.Upload

uc :: FilePath -> MediumID -> Integer -> UploadPart -> Uploader GoPro ()
uc fp mid partnum up@UploadPart{..} = do
  logDbg . T.pack $ "Uploading part " <> show _uploadPart <> " of " <> fp
  uploadChunk fp up
  completedUploadPart mid _uploadPart partnum
  logDbg . T.pack $ "Finished part " <> show _uploadPart <> " of " <> fp

runCreateUploads :: GoPro ()
runCreateUploads = do
  -- Exclude any commandline params for files that are already being
  -- uploaded.  This prevents duplicate uploads if you just hit
  -- up-enter, but it also prevents one from uploading a file if it's
  -- already included in a multipart upload.
  queued <- listQueuedFiles
  todo <- filter (`notElem` queued) <$> asks (optArgv . gpOptions)
  db <- goproDB
  c <- asks (optUploadConcurrency . gpOptions)
  mapConcurrentlyLimited_ c (upload db) todo

  where
    upload db fp = runUpload [fp] $ do
      setLogAction (logError . T.pack)
      mid <- createMedium
      did <- createSource 1
      fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
      up@Upload{..} <- createUpload did 1 (fromInteger fsize)
      logInfo $ mconcat ["Creating upload ", tshow fp, " (", tshow fsize, " bytes) as ",
                         mid, ": did=", did, ", upid=", _uploadID]
      liftIO . withDB db $ storeUpload fp mid up did 1

runCreateMultipart :: GoPro ()
runCreateMultipart = do
  (typ:fps) <- asks (optArgv . gpOptions)
  db <- goproDB
  runUpload fps $ do
    setMediumType (read typ)
    setLogAction (logError . T.pack)
    mid <- createMedium
    did <- createSource (length fps)
    c <- asks (optUploadConcurrency . gpOptions)
    mapConcurrentlyLimited_ c (\(fp,n) -> do
              fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
              up@(Upload{..}) <- createUpload did n (fromInteger fsize)
              logInfo $ mconcat ["Creating part ", tshow fp, " as ", mid, " part ", tshow n,
                                 ": did=", did, ", upid=", _uploadID]
              liftIO . withDB db $ storeUpload fp mid up did (fromIntegral n)
          ) $ zip fps [1..]
    logInfo $ "Multipart upload created.  Use the 'upload' command to complete the upload."

runResumeUpload :: GoPro ()
runResumeUpload = mapM_ upAll =<< listPartialUploads
  where
    upAll [] = pure ()

    upAll xs@(PartialUpload{..}:_) = do
      logInfo $ "Uploading " <> tshow _pu_medium_id <> " in " <> (tshow $ length xs) <> " segments"
      mapM_ up xs
      logInfo $ "Finished uploading " <> tshow _pu_medium_id
      resumeUpload [_pu_filename] _pu_medium_id $ markAvailable _pu_did

    up PartialUpload{..} = do
      let part = fromIntegral _pu_partnum
      fsize <- fromIntegral . fileSize <$> (liftIO . getFileStatus) _pu_filename
      resumeUpload [_pu_filename] _pu_medium_id $ do
        Upload{..} <- getUpload _pu_upid _pu_did part fsize
        let chunks = filter (\UploadPart{..} -> _uploadPart `elem` _pu_parts) _uploadParts
        logInfo $ mconcat ["Uploading ", tshow _pu_filename, " (", tshow fsize, " bytes) as ",
                           _pu_medium_id, ":", tshow part, ": did=",
                           _pu_did, ", upid=", _uploadID, ", parts=", tshow (length chunks)]
        c <- asks (optUploadConcurrency . gpOptions)
        _ <- mapConcurrentlyLimited c (uc _pu_filename _pu_medium_id _pu_partnum) chunks
        completeUpload _uploadID _pu_did part (fromIntegral fsize)
      completedUpload _pu_medium_id _pu_partnum
