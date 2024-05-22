module Data.Robustsort.Subalgorithms.Permutationsort (permutationsort) where

import Data.List (permutations)
import Data.Robustsort.Utils.Check (isSorted)

permutationsort :: [Int] -> [Int]
permutationsort xs = acc (permutations x) []
  where
    x = xs
    acc :: [[Int]] -> [Int] -> [Int]
    acc [] unsortedPermutations = permutationsort unsortedPermutations
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted permutation = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
