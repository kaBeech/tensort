module Robustsort.Bogosort (bogosort) where

import Robustsort.Check (isSorted)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

bogosort :: [Int] -> Int -> [Int]
bogosort elements seed
  | isSorted elements = elements
  | otherwise = bogosort (shuffle' elements (length elements) (mkStdGen seed)) (seed + 1)
