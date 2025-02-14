-- | This module provides the bogosort function for sorting lists
module Data.Tensort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.RandomizeList (randomizeList)

-- | Takes a list and returns a sorted list using a Bogosort algorithm.

-- | ==== __Examples__
-- >>> bogosort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> bogosort (SortRec [(1, 16), (5, 23), (2, 4), (3, 8), (0, 15), (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
bogosort :: (Ord a) => [a] -> [a]
bogosort = bogosortSeeded 143

-- | Takes a seed for use in random generation and a list and returns a
--  sorted list using a Bogosort algorithm.

-- | ==== __Examples__
-- >>> bogosortSeeded 42 (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> bogosortSeeded 24 (SortRec [(1, 16), (5, 23), (2, 4), (3, 8), (0, 15), (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
bogosortSeeded :: (Ord a) => Int -> [a] -> [a]
bogosortSeeded seed xs
  | isSorted xs = xs
  | otherwise = bogosortSeeded (seed + 1) (randomizeList seed xs)
