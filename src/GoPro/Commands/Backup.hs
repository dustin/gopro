module GoPro.Commands.Backup (runBackup) where


import           Conduit                 (sinkFile)
import           Control.Lens
import           Control.Monad           (void, when)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Reader    (asks)
import           Control.Monad.Trans.AWS (Credentials (..), Region (..),
                                          ToBody (..), envRegion, hashedFile,
                                          newEnv, paginate, runAWST,
                                          runResourceT, send)
import           Data.Conduit            (runConduit, runConduitRes, (.|))
import qualified Data.Conduit.List       as CL
import qualified Data.Set                as Set
import           Data.String             (fromString)
import           Data.Text               (Text, isSuffixOf, pack, unpack)
import           Network.AWS.S3
import           Network.HTTP.Simple     (getResponseBody, httpSource,
                                          parseRequest)
import           System.Directory        (createDirectoryIfMissing, removeFile)
import           System.FilePath.Posix   (takeBaseName, takeDirectory, (</>))

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media

type Derivative = (MediumID, Text)

allDerivatives :: BucketName -> GoPro [Derivative]
allDerivatives bucketName = do
  awsenv <- newEnv Discover <&> set envRegion NorthVirginia
  runResourceT . runAWST awsenv $
    runConduit $ paginate (listObjectsV2 bucketName & lovPrefix ?~ "derivatives/")
    .| CL.concatMap (view lovrsContents)
    .| CL.map (view (oKey . _ObjectKey))
    .| CL.filter (not . ("/" `isSuffixOf`))
    .| CL.map toDir
    .| CL.consume

  where toDir t = let s = unpack t in
                    (pack . takeBaseName . takeDirectory $ s, pack $ takeBaseName s)

storeDerivative :: BucketName -> MediumID -> String -> GoPro ()
storeDerivative bucketName mid d = do
  liftIO $ createDirectoryIfMissing True ".cache"
  Just var <- preview (fileStuff . variations . folded . filtered (has (var_label . only d))) <$> retrieve mid
  let u = var ^. var_url
      tmpfile = ".cache/" </> unpack mid <> "-" <> d <> "." <> (var ^. var_type)
      key = fromString ("derivatives/" <> unpack mid <> "/" <> d <> "." <> (var ^. var_type))
  req <- parseRequest u
  logDbg $ "Downloading " <> mid
  liftIO $ runConduitRes (httpSource req getResponseBody .| sinkFile tmpfile)
  awsenv <- newEnv Discover <&> set envRegion NorthVirginia
  logDbg $ "Uploading " <> mid
  runResourceT . runAWST awsenv $ do
    hf <- hashedFile tmpfile
    void . send $ putObject bucketName key (toBody hf) & poStorageClass ?~ StandardIA
  liftIO $ removeFile tmpfile

runBackup :: GoPro ()
runBackup = do
  args <- asks (optArgv . gpOptions)
  when (length args /= 1) $ fail "A bucket name must be specified"
  let [bucketName] = fromString <$> args
  have <- Set.fromList . fmap fst <$>  allDerivatives bucketName
  logDbg $ "have: " <> (pack . show) have
  want <- Set.fromList <$> loadMediaIDs
  let todo = Set.toList (want `Set.difference` have)
  logDbg $ "todo: " <> (pack.show) todo
  c <- asks (optUploadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (\mid -> storeDerivative bucketName mid "source") todo
