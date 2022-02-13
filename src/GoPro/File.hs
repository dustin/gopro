module GoPro.File where

{-
https://community.gopro.com/t5/en/GoPro-Camera-File-Naming-Convention/ta-p/390220
-}

import           Data.Char             (toLower)
import           Data.List             (groupBy, sortOn)
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (fromMaybe, mapMaybe)
import           System.FilePath.Posix (takeBaseName, takeExtension)
import           Text.Read             (readMaybe)

data VideoCodec = GoProAVC | GoProHEVC | GoProJPG deriving (Eq, Show, Bounded, Enum)

data File = File {
  _gpFilePath   :: FilePath,
  _gpCodec      :: VideoCodec,
  _gpFileNumber :: Int,
  _gpChapter    :: Int
  } deriving (Eq, Show)

parseGPFileName :: FilePath -> Maybe File
parseGPFileName fn
  | fmap toLower (takeExtension fn) == ".mp4" =
      case takeBaseName fn of
        ('G':'H':a:b:n) -> File fn GoProAVC <$> readMaybe n <*> readMaybe [a,b]
        ('G':'X':a:b:n) -> File fn GoProHEVC <$> readMaybe n <*> readMaybe [a,b]
        _               -> Nothing
  | fmap toLower (takeExtension fn) == ".jpg" =
    case takeBaseName fn of
      ('G':a:b:c:n) -> File fn GoProJPG <$> readMaybe n <*> pure (fromMaybe 0 (readMaybe [a,b,c]))
      _             -> Nothing
parseGPFileName _ = Nothing

parseAndGroup :: [FilePath] -> [NonEmpty File]
parseAndGroup = mapMaybe NE.nonEmpty . groupFiles . sortFiles . mapMaybe parseGPFileName
  where
    sortFiles = sortOn (\x -> (_gpFileNumber x, _gpChapter x))
    groupFiles = groupBy (\a b -> _gpFileNumber a == _gpFileNumber b)
