module Data.Tensort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..))

bogosort :: Sortable -> Sortable
bogosort xs = bogosortSeeded xs 143

bogosortSeeded :: Sortable -> Int -> Sortable
bogosortSeeded xs seed
  | isSorted xs = xs
  | otherwise = bogosortSeeded (randomizeList xs seed) (seed + 1)
