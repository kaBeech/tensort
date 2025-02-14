-- | This module provides a function to split a list into chunks of a given
--   size.
module Data.Tensort.Utils.Split (splitEvery) where

-- | Split a list into chunks of a given size.

-- | ==== __Examples__
--   >>> splitEvery 2 ([1,2,3,4,5,6] :: [Int])
--   [[1,2],[3,4],[5,6]]
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = xs1 : splitEvery n xs2
  where
    (xs1, xs2) = splitAt n xs
