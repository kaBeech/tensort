-- | This module provides functions for calculating the natural logarithms in
--   a way useful for creating logarithmic Bytesizes.
module Data.Tensort.Utils.LogNat (getLnBytesize, getLn) where

import Data.Tensort.Utils.Types (Sortable (..))

-- | Calculates a suitable logarithmic Bytesize for a given Sortable list.

-- | ==== __Examples__
-- >>> getLnBytesize (SortBit [1 .. 27])
-- 4
--
-- >>> getLnBytesize  (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- 2
getLnBytesize :: Sortable -> Int
getLnBytesize (SortBit xs) = getLn (length xs)
getLnBytesize (SortRec xs) = getLn (length xs)

-- | Calculates the natural logarithm of an integer, rounded up to the nearest
--   integer.
--
-- | ==== __Examples__
-- >>> getLn 27
-- 4
getLn :: Int -> Int
getLn x = ceiling (log (fromIntegral x) :: Double)
