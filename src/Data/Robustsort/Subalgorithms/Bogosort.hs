module Data.Robustsort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Robustsort.Utils.Check (isSorted)
import Data.Robustsort.Utils.RandomizeList (randomizeList)
import Data.Robustsort.Utils.Types (Sortable (..))

bogosort :: Sortable -> Sortable
bogosort xs = bogosortSeeded xs 143

bogosortSeeded :: Sortable -> Int -> Sortable
bogosortSeeded xs seed
  | isSorted xs = xs
  | otherwise = bogosortSeeded (randomizeList xs seed) (seed + 1)
