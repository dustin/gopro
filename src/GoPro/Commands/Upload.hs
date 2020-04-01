{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Upload (
  runUploadFiles, runUploadMultipart, runResumeUpload
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import qualified Data.Text              as T
import           System.Posix.Files     (fileSize, getFileStatus)

import           GoPro.Commands
import           GoPro.Plus.Upload


uc :: FilePath -> UploadPart -> Uploader GoPro ()
uc fp up@UploadPart{..} = do
  logDbg . T.pack $ "Uploading part " <> show _uploadPart <> " of " <> fp
  uploadChunk fp up
  logDbg . T.pack $ "Finished part " <> show _uploadPart <> " of " <> fp


runUploadFiles :: GoPro ()
runUploadFiles = mapM_ upload =<< asks (optArgv . gpOptions)

  where
    upload fp = runUpload [fp] $ do
      setLogAction (logError . T.pack)
      mid <- createMedium
      did <- createSource 1
      fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
      Upload{..} <- createUpload did 1 (fromInteger fsize)
      logInfo $ "Uploading " <> tshow fp <> " as " <> mid <> ": did=" <> did <> ", upid=" <> _uploadID
      c <- asks (optUploadConcurrency . gpOptions)
      _ <- mapConcurrentlyLimited c (uc fp) _uploadParts
      completeUpload _uploadID did 1 fsize
      markAvailable did

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
              _ <- mapConcurrentlyLimited 2 (uc fp) _uploadParts
              completeUpload _uploadID did n fsize
          ) $ zip fps [1..]
    markAvailable did

runResumeUpload :: GoPro ()
runResumeUpload = do
  (mids:upids:dids:parts:filename:todos) <- asks (optArgv . gpOptions)
  let part = read parts
      did = T.pack dids
      todo = read <$> todos
  fsize <- fromIntegral . fileSize <$> (liftIO . getFileStatus) filename
  resumeUpload [filename] (T.pack mids) $ do
    Upload{..} <- getUpload (T.pack upids) did part fsize
    let chunks = filter (\UploadPart{..} -> _uploadPart `notElem` todo) _uploadParts
    c <- asks (optUploadConcurrency . gpOptions)
    _ <- mapConcurrentlyLimited c (uc filename) chunks
    completeUpload _uploadID did part (fromIntegral fsize)
    markAvailable did
