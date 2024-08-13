module Data.Tensort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..))

bogosort :: Sortable -> Sortable
bogosort = bogosortSeeded 143

bogosortSeeded :: Int -> Sortable -> Sortable
bogosortSeeded seed xs
  | isSorted xs = xs
  | otherwise = bogosortSeeded (seed + 1) (randomizeList seed xs)
