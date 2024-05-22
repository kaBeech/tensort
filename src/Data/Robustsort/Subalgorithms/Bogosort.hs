module Data.Robustsort.Subalgorithms.Bogosort (bogosort) where

import Data.Robustsort.Utils.Check (isSorted)
import Data.Robustsort.Utils.RandomizeList (randomizeList)

bogosort :: [Int] -> Int -> [Int]
bogosort xs seed
  | isSorted xs = xs
  | otherwise = bogosort (randomizeList xs seed) (seed + 1)
