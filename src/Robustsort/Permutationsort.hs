module Robustsort.Permutationsort (permutationsort) where

import Data.List (permutations)
import Robustsort.Check (isSorted)

permutationsort :: [Int] -> [Int]
permutationsort elements = acc (permutations element) []
  where
    element = elements
    acc :: [[Int]] -> [Int] -> [Int]
    acc [] unsortedPermutations = permutationsort unsortedPermutations
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted permutation = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
