module FFMPeg where

import           Control.Lens
import           Data.Aeson           (Value (..), decode)
import           Data.Aeson.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           System.Process

readCmd :: FilePath -> [String] -> (BS.ByteString -> a) -> IO a
readCmd c a f = withCreateProcess (proc c a) { std_out = CreatePipe } $
    \_ (Just hout) _ _ -> do
      f <$> BS.hGetContents hout

findGPMDStream :: FilePath -> IO (Maybe Int)
findGPMDStream filename =
  readCmd "ffprobe" ["-show_streams", "-loglevel", "-8", "-print_format", "json", filename] $ \oot ->
    let j = decode (BL.fromStrict oot) :: Maybe Value in

    j ^? _Just . key "streams" . values . filtered (has (key "codec_tag_string" . _String.only "gpmd"))
      . key "index" . _Integer . to fromIntegral

extractGPMDStream :: FilePath -> Int -> IO BS.ByteString
extractGPMDStream filename stream = readCmd "ffmpeg" ["-y", "-i", filename, "-loglevel", "-8",
                                                      "-codec", "copy", "-map", "0:" <> show stream,
                                                      "-f", "rawvideo", "-"] id

