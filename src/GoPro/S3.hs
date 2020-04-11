module GoPro.S3 where

import           Codec.Compression.GZip  (compress)
import           Control.Lens
import           Control.Monad           (void)
import           Control.Monad.Trans.AWS (Credentials (..), Region (..),
                                          envRegion, newEnv, paginate, runAWST,
                                          runResourceT, send)
import qualified Data.ByteString.Lazy    as BL
import           Data.Conduit            (runConduit, (.|))
import qualified Data.Conduit.List       as CL
import           Data.String             (fromString)
import           Data.Text               (Text, isSuffixOf, pack, unpack)
import           Network.AWS.Data.Body   (RqBody (..), ToHashedBody (..))
import           Network.AWS.S3
import           System.FilePath.Posix   (takeBaseName, takeDirectory)

import           GoPro.Commands
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

storeMetaBlob :: BucketName -> MediumID -> BL.ByteString -> GoPro ()
storeMetaBlob bucketName mid blob = do
  let key = fromString $ "metablob/" <> unpack mid <> ".gz"
  awsenv <- newEnv Discover <&> set envRegion Oregon
  logInfo $ "Storing metadata blob at " <> tshow key
  runResourceT . runAWST awsenv $ void . send $ putObject bucketName key (Hashed . toHashed . compress $ blob)
