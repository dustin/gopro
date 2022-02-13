module GoPro.File where

{-
https://community.gopro.com/t5/en/GoPro-Camera-File-Naming-Convention/ta-p/390220
-}

import           Data.Char               (toUpper)
import           Data.List               (groupBy, sortOn)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import           Data.Maybe              (fromMaybe)
import           Data.Semigroup.Foldable (foldMap1)
import           Data.These              (These (..))
import           System.FilePath.Posix   (takeFileName)
import           Text.Read               (readMaybe)

data VideoCodec = GoProAVC | GoProHEVC | GoProJPG deriving (Eq, Show, Bounded, Enum)

data File = File {
  _gpFilePath   :: FilePath,
  _gpCodec      :: VideoCodec,
  _gpFileNumber :: Int,
  _gpChapter    :: Int
  } deriving (Eq, Show)

parseGPFileName :: FilePath -> Maybe File
parseGPFileName fn =
  case fmap toUpper (takeFileName fn) of
    ('G':'H':a:b:w:x:y:z:".MP4") -> File fn GoProAVC <$> readMaybe [w,x,y,z] <*> readMaybe [a,b]
    ('G':'X':a:b:w:x:y:z:".MP4") -> File fn GoProHEVC <$> readMaybe [w,x,y,z] <*> readMaybe [a,b]
    ('G': a: b:c:w:x:y:z:".JPG") -> File fn GoProJPG <$> readMaybe [w,x,y,z] <*> pure (fromMaybe 0 (readMaybe [a,b,c]))
    _                            -> Nothing

-- | Parse a list of file paths into grouped files, rejecting those that don't match known patterns.
parseAndGroup :: NonEmpty FilePath -> These [FilePath] [NonEmpty File]
parseAndGroup = fmap (fmap NE.fromList . groupFiles . sortFiles) . foldMap1 that
  where
    that fp = maybe (This [fp]) (That . (:[])) (parseGPFileName fp)
    sortFiles = sortOn (\x -> (_gpFileNumber x, _gpChapter x))
    groupFiles = groupBy (\a b -> _gpFileNumber a == _gpFileNumber b)
