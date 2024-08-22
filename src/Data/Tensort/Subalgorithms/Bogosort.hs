-- | This module provides the bogosort function for sorting lists using the
--   Sortable type
module Data.Tensort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

-- | Takes a Sortable and returns a sorted Sortable using a Bogosort algorithm
--   using the default seed for random generation

-- | ==== __Examples__
-- >>> bogosort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> bogosort (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
bogosort :: WonkyState -> Sortable -> (Sortable, WonkyState)
bogosort = bogosortSeeded 143

bogosortSeeded :: Int -> WonkyState -> Sortable -> (Sortable, WonkyState)
bogosortSeeded seed wonkySt xs = do
  let (result, wonkySt') = isSorted xs wonkySt
  if result
    then (xs, wonkySt')
    else bogosortSeeded (seed + 1) wonkySt' (randomizeList seed xs)
