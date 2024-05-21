module Robustsort.Bogosort (bogosort) where

import Robustsort.Utils.Check (isSorted)
import Robustsort.Utils.RandomizeList (randomizeList)

bogosort :: [Int] -> Int -> [Int]
bogosort xs seed
  | isSorted xs = xs
  | otherwise = bogosort (randomizeList xs seed) (seed + 1)
