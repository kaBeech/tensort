module Robustsort.Bogosort (bogosort) where

import Robustsort.Check (isSorted)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

bogosort :: [Int] -> Int -> [Int]
bogosort xs seed
  | isSorted xs = xs
  | otherwise = bogosort (shuffle' xs (length xs) (mkStdGen seed)) (seed + 1)
