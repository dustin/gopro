module GoPro.File where

{-
https://community.gopro.com/t5/en/GoPro-Camera-File-Naming-Convention/ta-p/390220
-}

import           Data.Char               (toUpper)
import           Data.List               (groupBy, sortOn)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import           Data.Semigroup.Foldable (foldMap1)
import           Data.These              (These (..))
import           System.FilePath.Posix   (takeFileName)
import           Text.Read               (readMaybe)

data Grouping = NoGrouping Int          -- filenum
              | BasicGrouping Int Int   -- filenum, group
              | LoopGrouping Int String -- filenum, group
              deriving (Eq, Show)

instance Ord Grouping where
  a <= b = i a <= i b
    where
      i (NoGrouping x) = ("", x)
      i (BasicGrouping x g) = (show g, x)
      i (LoopGrouping x g) = (g, x)

data VideoCodec = GoProAVC | GoProHEVC | GoProJPG deriving (Eq, Show, Bounded, Enum)

data File = File {
  _gpFilePath   :: FilePath,
  _gpCodec      :: VideoCodec,
  _gpGrouping   :: Grouping
  } deriving (Eq, Show)

sameGroup :: Grouping -> Grouping -> Bool
sameGroup (BasicGrouping _ a) (BasicGrouping _ b) = a == b
sameGroup (LoopGrouping _ a) (LoopGrouping _ b) = a == b
sameGroup _ _ = False

parseGPFileName :: FilePath -> Maybe File
parseGPFileName fn =
  case fmap toUpper (takeFileName fn) of
    ('G':'H':a:b:w:x:y:z:".MP4") -> vid GoProAVC [a,b] [w,x,y,z]
    ('G':'X':a:b:w:x:y:z:".MP4") -> vid GoProHEVC [a,b] [w,x,y,z]
    ('G': _: _:_:w:x:y:z:".JPG") -> File fn GoProJPG <$> (NoGrouping <$> readMaybe [w,x,y,z])
    _                            -> Nothing

  where vid x ab wxyz = case readMaybe ab of
                          Just n -> File fn x <$> (BasicGrouping n <$> readMaybe wxyz)
                          Nothing -> File fn x <$> (LoopGrouping <$> readMaybe wxyz <*> pure ab)

-- | Parse a list of file paths into grouped files, rejecting those that don't match known patterns.
parseAndGroup :: NonEmpty FilePath -> These [FilePath] [NonEmpty File]
parseAndGroup = fmap (fmap NE.fromList . groupFiles . sortFiles) . foldMap1 that
  where
    that fp = maybe (This [fp]) (That . (:[])) (parseGPFileName fp)
    sortFiles = sortOn _gpGrouping
    groupFiles = groupBy (\a b -> sameGroup (_gpGrouping a) (_gpGrouping b))
