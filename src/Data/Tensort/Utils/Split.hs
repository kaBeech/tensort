-- | This module provides a function to split a list into chunks of a given
--   size.
module Data.Tensort.Utils.Split (splitEvery) where

-- | Split a list into chunks of a given size.

-- | ==== __Examples__
--   >>> splitEvery 2 [1,2,3,4,5,6]
--   [[1,2],[3,4],[5,6]]
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)
