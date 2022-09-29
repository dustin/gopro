{-# LANGUAGE TupleSections #-}

module GoPro.File where

{-
https://community.gopro.com/t5/en/GoPro-Camera-File-Naming-Convention/ta-p/390220
-}

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Char                 (toUpper)
import           Data.List                 (groupBy, intercalate, isInfixOf, isSuffixOf, sortOn)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.List.Split           (splitOn)
import           Data.Map                  (Map)
import qualified Data.Map.Strict           as Map
import           Data.Semigroup.Foldable   (foldMap1)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.These                (These (..), these)
import           System.Directory.PathWalk (pathWalkAccumulate)
import           System.FilePath.Posix     (takeDirectory, takeFileName, (</>))
import           Text.Read                 (readMaybe)

data Grouping = NoGrouping Int String   -- filenum, prefix
              | BasicGrouping Int Int   -- filenum, group
              | LoopGrouping Int String -- filenum, group
              deriving (Eq, Show)

instance Ord Grouping where
  a <= b = i a <= i b
    where
      i (NoGrouping x _)    = ("", x)
      i (BasicGrouping x g) = (show g, x)
      i (LoopGrouping x g)  = (g, x)

nextGrouping :: Grouping -> Grouping
nextGrouping (NoGrouping x p)    = NoGrouping (succ x) p
nextGrouping (BasicGrouping x g) = BasicGrouping (succ x) g
nextGrouping (LoopGrouping x g)  = LoopGrouping (succ x) g

data VideoCodec = GoProAVC | GoProHEVC | GoProJPG deriving (Eq, Show, Bounded, Enum)

data File = File {
  _gpFilePath :: FilePath,
  _gpCodec    :: VideoCodec,
  _gpGrouping :: Grouping
  } deriving (Eq, Show)

nextFile :: File -> File
nextFile f@File{..} = f{_gpGrouping=next, _gpFilePath=nextF}
  where
    next = nextGrouping _gpGrouping
    nextF = takeDirectory _gpFilePath
      </> if "GOPR" `isInfixOf` _gpFilePath
          then replace (ident _gpGrouping) (ident next) (replace "GOPR" "GH01" (takeFileName _gpFilePath))
          else replace (ident _gpGrouping) (ident next) (takeFileName _gpFilePath)

    ident (NoGrouping x p)    = p <> padded 6 x
    ident (BasicGrouping x g) = fpre <> padded 2 x <> padded 4 g
    ident (LoopGrouping x g)  = fpre <> g <> padded 4 x

    fpre = case _gpCodec of
      GoProAVC  -> "H"
      GoProHEVC -> "X"
      GoProJPG  -> "OPR"

    replace from to = intercalate to . splitOn from

    padded s x = prefix "0" s (show x)
    prefix p n x
      | length x == n = x
      | otherwise = prefix p n (p <> x)

sameGroup :: Grouping -> Grouping -> Bool
sameGroup (BasicGrouping _ a) (BasicGrouping _ b) = a == b
sameGroup (LoopGrouping _ a) (LoopGrouping _ b)   = a == b
sameGroup _ _                                     = False

parseGPFileName :: FilePath -> Maybe File
parseGPFileName fn =
  case fmap toUpper (takeFileName fn) of
    ('G':'H':a:b:w:x:y:z:".MP4")     -> vid GoProAVC [a,b] [w,x,y,z]
    ('G':'X':a:b:w:x:y:z:".MP4")     -> vid GoProHEVC [a,b] [w,x,y,z]
    ('G': a:b:c:w:x:y:z:".JPG")     -> File fn GoProJPG <$> (NoGrouping <$> readMaybe [w,x,y,z] <*> pure [a,b,c])
    ('G':'O':'P':'R':w:x:y:z:".MP4") -> vid GoProAVC "1" [w,x,y,z]
    ('G':'P':a:b:w:x:y:z:".MP4")     -> vid GoProAVC [a,b] [w,x,y,z]
    other                            -> if ".JPG" `isSuffixOf` other then
                                          Just (File fn GoProJPG (NoGrouping 0 ""))
                                        else
                                          Nothing

  where vid x ab wxyz = case readMaybe ab of
                          Just n  -> File fn x <$> (BasicGrouping n <$> readMaybe wxyz)
                          Nothing -> File fn x <$> (LoopGrouping <$> readMaybe wxyz <*> pure ab)

-- | Parse a list of file paths into grouped files, rejecting those that don't match known patterns.
parseAndGroup :: NonEmpty FilePath -> These [FilePath] [NonEmpty File]
parseAndGroup = fmap (fmap NE.fromList . groupFiles . sortFiles) . foldMap1 that
  where
    that fp = maybe (This [fp]) (That . (:[])) (parseGPFileName fp)
    sortFiles = sortOn (\f -> (dirName f, _gpGrouping f))
    groupFiles = groupBy (\a b -> dirName a == dirName b && sameGroup (_gpGrouping a) (_gpGrouping b))
    dirName = takeDirectory . _gpFilePath

-- | Given a directory that may contain GoPro source media, return a
-- map of all of the principal filename which is encoded in metadata.
--
-- This is useful for mapping file media to source to perform backups
-- without having to re-download source data you already have locally.
fromDirectory :: MonadIO m => FilePath -> m (Map String (NonEmpty File))
fromDirectory dir = do
  files <- Set.toList <$> findFiles dir
  let (_, good) = these (,[]) ([],) (,) $ maybe (This []) parseAndGroup (NE.nonEmpty files)
  pure $ Map.fromList [(takeFileName (_gpFilePath (NE.head f)), f) | f <- good]

-- | This is the same as 'fromDirectory', but indexes *every* file
-- instead of just the one that it may consider primary since looping
-- videos behave differently.
fromDirectoryFull :: MonadIO m => FilePath -> m (Map String (NonEmpty File))
fromDirectoryFull = fmap mesh . fromDirectory
  where
    mesh = Map.fromList . foldMap rekey . Map.elems
    rekey ds = (\d -> (takeFileName (_gpFilePath d), ds)) <$> NE.toList ds

-- | Find the Set of all files under the given file path.
findFiles :: MonadIO m => FilePath -> m (Set FilePath)
findFiles dir =  pathWalkAccumulate dir (\d _ fs -> pure (Set.fromList ((d </>) <$> fs)))
