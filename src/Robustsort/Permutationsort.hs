module Robustsort.Permutationsort (permutationsort) where

import Data.List (permutations)
import Robustsort.Utils.Check (isSorted)

permutationsort :: [Int] -> [Int]
permutationsort xs = acc (permutations x) []
  where
    x = xs
    acc :: [[Int]] -> [Int] -> [Int]
    acc [] unsortedPermutations = permutationsort unsortedPermutations
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted permutation = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
