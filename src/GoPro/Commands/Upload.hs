{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Upload (
  runUploadFiles, runUploadMultipart
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import qualified Data.Text              as T
import           System.Posix.Files     (fileSize, getFileStatus)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media       (MediumID)
import           GoPro.Plus.Upload

uc :: FilePath -> MediumID -> UploadPart -> Uploader GoPro ()
uc fp mid up@UploadPart{..} = do
  logDbg . T.pack $ "Uploading part " <> show _uploadPart <> " of " <> fp
  uploadChunk fp up
  completedUploadPart mid _uploadPart
  logDbg . T.pack $ "Finished part " <> show _uploadPart <> " of " <> fp

runUploadFiles :: GoPro ()
runUploadFiles = do
  db <- goproDB
  mapM_ (upload db) =<< asks (optArgv . gpOptions)
  runResumeUpload

  where
    upload db fp = runUpload [fp] $ do
      setLogAction (logError . T.pack)
      mid <- createMedium
      did <- createSource 1
      fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
      up@Upload{..} <- createUpload did 1 (fromInteger fsize)
      logInfo $ "Creating upload " <> tshow fp <> " (" <> tshow fsize <> " bytes) as " <> mid <> ": did=" <> did <> ", upid=" <> _uploadID
      liftIO . withDB db $ storeUpload fp mid up did 1

runUploadMultipart :: GoPro ()
runUploadMultipart = do
  (typ:fps) <- asks (optArgv . gpOptions)
  runUpload fps $ do
    setMediumType (read typ)
    setLogAction (logError . T.pack)
    mid <- createMedium

    did <- createSource (length fps)
    c <- asks (optUploadConcurrency . gpOptions)
    _ <- mapConcurrentlyLimited c (\(fp,n) -> do
              fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
              Upload{..} <- createUpload did n (fromInteger fsize)
              logInfo $ mconcat ["Uploading ", tshow fp, " as ", mid, " part ", tshow n,
                                 ": did=", did, ", upid=", _uploadID]
              _ <- mapConcurrentlyLimited 2 (uc fp mid) _uploadParts
              completeUpload _uploadID did n fsize
          ) $ zip fps [1..]
    markAvailable did

runResumeUpload :: GoPro ()
runResumeUpload = mapM_ up =<< listPartialUploads
  where
    up PartialUpload{..} = do
      let part = fromIntegral _pu_partnum
      fsize <- fromIntegral . fileSize <$> (liftIO . getFileStatus) _pu_filename
      resumeUpload [_pu_filename] _pu_medium_id $ do
        Upload{..} <- getUpload _pu_upid _pu_did part fsize
        let chunks = filter (\UploadPart{..} -> _uploadPart `elem` _pu_parts) _uploadParts
        logInfo $ mconcat ["Uploading ", tshow _pu_filename, " (", tshow fsize, " bytes) as ",
                           _pu_medium_id, ": did=", _pu_did <> ", upid=", _uploadID,
                           ", parts=", tshow (length chunks)]
        c <- asks (optUploadConcurrency . gpOptions)
        _ <- mapConcurrentlyLimited c (uc _pu_filename _pu_medium_id) chunks
        completeUpload _uploadID _pu_did part (fromIntegral fsize)
        markAvailable _pu_did
      completedUpload _pu_medium_id
