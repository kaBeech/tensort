-- | This module provides the bogosort function for sorting lists using the
--   Sortable type
module Data.Tensort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..))

-- | Takes a Sortable and returns a sorted Sortable using a Bogosort algorithm
--   using the default seed for random generation

-- | ==== __Examples__
-- >>> bogosort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> bogosort (SortRec [(16, 23), (4, 8), (15, 42)])
-- SortRec [(4,8),(16,23),(15,42)]
bogosort :: Sortable -> Sortable
bogosort = bogosortSeeded 143

-- | Takes a seed for use in random generation and a Sortable and returns a
--  sorted Sortable using a Bogosort algorithm

-- | ==== __Examples__
-- >>> bogosortSeeded 42 (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> bogosortSeeded 24 (SortRec [(16, 23), (4, 8), (15, 42)])
-- SortRec [(4,8),(16,23),(15,42)]
bogosortSeeded :: Int -> Sortable -> Sortable
bogosortSeeded seed xs
  | isSorted xs = xs
  | otherwise = bogosortSeeded (seed + 1) (randomizeList seed xs)
