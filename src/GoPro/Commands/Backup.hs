module GoPro.Commands.Backup (runBackup) where


import           Control.Lens
import           Control.Monad           (void, when)
import           Control.Monad.Reader    (asks)
import           Control.Monad.Trans.AWS (Credentials (..), Region (..),
                                          envRegion, newEnv, paginate, runAWST,
                                          runResourceT, send)
import qualified Data.Aeson              as J
import qualified Data.ByteString.Lazy    as BL
import           Data.Conduit            (runConduit, (.|))
import qualified Data.Conduit.List       as CL
import qualified Data.Set                as Set
import           Data.String             (fromString)
import           Data.Text               (Text, isSuffixOf, pack, unpack)
import qualified Data.Text.Encoding      as TE
import           Network.AWS.S3
import           Network.AWS.SQS         (sendMessage)
import           System.FilePath.Posix   (takeBaseName, takeDirectory)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media

type Derivative = (MediumID, Text)

allDerivatives :: BucketName -> GoPro [Derivative]
allDerivatives bucketName = do
  awsenv <- newEnv Discover <&> set envRegion Oregon
  runResourceT . runAWST awsenv $
    runConduit $ paginate (listObjectsV2 bucketName & lovPrefix ?~ "derivatives/")
    .| CL.concatMap (view lovrsContents)
    .| CL.map (view (oKey . _ObjectKey))
    .| CL.filter (not . ("/" `isSuffixOf`))
    .| CL.map toDir
    .| CL.consume

  where toDir t = let s = unpack t in
                    (pack . takeBaseName . takeDirectory $ s, pack $ takeBaseName s)

type QueueURL = Text

storeDerivative :: QueueURL -> BucketName ->  MediumID -> String -> GoPro ()
storeDerivative qrl (BucketName bucketName) mid d = do
  Just var <- preview (fileStuff . variations . folded . filtered (has (var_label . only d))) <$> retrieve mid
  let u = var ^. var_url
      key = fromString ("derivatives/" <> unpack mid <> "/" <> d <> "." <> (var ^. var_type))
  logInfo $ "Queueing copy of " <>  mid
  awsenv <- newEnv Discover <&> set envRegion Oregon
  runResourceT . runAWST awsenv $
    void . send $ sendMessage qrl (encodeCopyRequest (pack u) bucketName key)

      where
        encodeCopyRequest src bname key = TE.decodeUtf8 (BL.toStrict . J.encode $ jbod)
          where
            dest = J.Object $ (mempty & at "bucket" ?~ J.String bname
                               & at "key" ?~ J.String key)
            jbod = J.Object $ (mempty & at "src" ?~ J.String src
                               & at "dest" ?~ dest)

runBackup :: GoPro ()
runBackup = do
  args <- asks (optArgv . gpOptions)
  when (length args /= 2) $ fail "A SQS URL and bucket name must be specified"
  let [qrl, bucketName] = args
  have <- Set.fromList . fmap fst <$>  allDerivatives (fromString bucketName)
  logDbg $ "have: " <> (pack . show) have
  want <- Set.fromList <$> loadMediaIDs
  let todo = take 25 $  Set.toList (want `Set.difference` have)
  logDbg $ "todo: " <> (pack.show) todo
  c <- asks (optUploadConcurrency . gpOptions)
  void $ mapConcurrentlyLimited c (\mid -> storeDerivative (pack qrl) (fromString bucketName) mid "source") todo
