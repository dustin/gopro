{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module GoPro.File where

{-
https://community.gopro.com/t5/en/GoPro-Camera-File-Naming-Convention/ta-p/390220
-}

import           Control.Lens                    hiding (from, to)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Writer.Lazy (execWriterT, tell)
import           Data.Char                       (toLower, toUpper)
import           Data.List                       (groupBy, intercalate, isInfixOf, isSuffixOf, sortOn)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import           Data.List.Split                 (splitOn)
import           Data.Map                        (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Semigroup.Foldable         (foldMap1)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.These                      (These (..), these)

import           Data.Functor                    (($>))
import           System.Directory.PathWalk       (WalkStatus (..), pathWalkInterruptible)
import           System.FilePath.Posix           (takeDirectory, takeFileName, (</>))
import           Text.Read                       (readMaybe)

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

data VideoCodec = GoProAVC | GoProHEVC | GoPro360 | GoProJPG deriving (Eq, Show, Bounded, Enum)

data File = File {
  _gpFilePath :: FilePath,
  _gpCodec    :: VideoCodec,
  _gpGrouping :: Grouping
  } deriving (Eq, Show)

makeLenses ''File

nextGrouping :: Grouping -> Grouping
nextGrouping = over grouping succ

grouping :: Lens' Grouping Int
grouping = lens r w
  where
    r (NoGrouping x _)    = x
    r (BasicGrouping x _) = x
    r (LoopGrouping x _)  = x

    w (NoGrouping _ p)    x = NoGrouping x p
    w (BasicGrouping _ g) x = BasicGrouping x g
    w (LoopGrouping _ g)  x = LoopGrouping x g

nextFile :: File -> File
nextFile = over fileNum succ

fileAt :: File -> Int -> File
fileAt f 0 = f -- there's no such thing as a zeroth file
fileAt f 1 = f -- Assume we're handed the first file
fileAt f@File{..} n = f{_gpGrouping=next, _gpFilePath=pathAt}
  where
    next = _gpGrouping & grouping .~ n
    pathAt = takeDirectory _gpFilePath
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
      GoPro360  -> "S"

    replace from to = intercalate to . splitOn from

    padded s x = prefix "0" s (show x)
    prefix p pn x
      | length x == pn = x
      | otherwise = prefix p pn (p <> x)

fileNum :: Lens' File Int
fileNum = lens (view (gpGrouping . grouping)) fileAt

sameGroup :: Grouping -> Grouping -> Bool
sameGroup (BasicGrouping _ a) (BasicGrouping _ b) = a == b
sameGroup (LoopGrouping _ a) (LoopGrouping _ b)   = a == b
sameGroup _ _                                     = False

parseGPFileName :: FilePath -> Maybe File
parseGPFileName fn =
  case fmap toUpper (takeFileName fn) of
    ('G':'H':a:b:w:x:y:z:".MP4")     -> vid GoProAVC [a,b] [w,x,y,z]
    ('G':'X':a:b:w:x:y:z:".MP4")     -> vid GoProHEVC [a,b] [w,x,y,z]
    ('G':'S':a:b:w:x:y:z:".360")     -> vid GoPro360 [a,b] [w,x,y,z]
    ('G': a:b:c:w:x:y:z:".JPG")      -> File fn GoProJPG <$> (NoGrouping <$> readMaybe [w,x,y,z] <*> pure [a,b,c])
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
-- This ignores any directory named "proxy"
findFiles :: MonadIO m => FilePath -> m (Set FilePath)
findFiles dir =  seek dir (\d _ fs -> pure (Set.fromList ((d </>) <$> fs)))

  where
    seek root callback = execWriterT $ pathWalkInterruptible root $ \d dirs files ->
                                          case takeFileName (map toLower d) of
                                            "proxy" -> pure StopRecursing
                                            _       -> (lift (callback d dirs files) >>= tell) $> Continue
