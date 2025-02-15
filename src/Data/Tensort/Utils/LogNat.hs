-- | This module provides functions for calculating the natural logarithms in
--   a way useful for creating logarithmic Bytesizes.
module Data.Tensort.Utils.LogNat (getLnLength, getLn) where

-- | Calculates a suitable logarithmic Bytesize for a given list.

-- | ==== __Examples__
-- >>> getLnLength ([1 .. 27] :: [Int])
-- 4
--
-- >>> getLnLength  ([(1, 16), (5, 23), (2, 4), (3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- 2
getLnLength :: (Ord a) => [a] -> Int
getLnLength xs = getLn $ length xs

-- | Calculates the natural logarithm of an integer, rounded up to the nearest
--   integer.
--
-- | ==== __Examples__
-- >>> getLn (27 :: Int)
-- 4
getLn :: (Ord a, Integral a) => a -> a
getLn x = ceiling $ log (fromIntegral x :: Double)
