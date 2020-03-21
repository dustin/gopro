module Exif where

import           Data.Bool            (bool)
import qualified Data.ByteString.Lazy as BL
import           Graphics.HsExif

-- | 'binSearch' performs a binary search to find the boundary
-- function where a function returns its highest 'LT' value.
binSearch :: Integral a => (a -> Ordering) -> a -> a -> a
binSearch f l h
  | h < l     = l
  | v == GT   = binSearch f l (mid-1)
  | v == LT   = binSearch f (mid+1) h
  | otherwise = mid
  where
    mid = l + (h-l) `div` 2
    v = f mid

-- | Extract the smallest subset of the input that can reconstruct the same EXIF data.
minimalEXIF :: BL.ByteString -> Either String BL.ByteString
minimalEXIF orig = ml . minlen <$> parseExif orig
  where
    ml x = BL.take (1+x) orig
    minlen oe = binSearch (\x -> same (parseExif (BL.take x orig))) 0 (BL.length orig)
      where
        same = bool LT GT . (== Right oe)
