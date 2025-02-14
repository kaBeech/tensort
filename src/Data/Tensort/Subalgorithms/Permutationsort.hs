-- | This module provides the permutationsort function for sorting lists
module Data.Tensort.Subalgorithms.Permutationsort (permutationsort) where

import Data.List (permutations)
import Data.Tensort.Utils.Check (isSorted)

-- | Takes a list and returns a sorted list using Permutationsort
--   algorithm

-- | ==== __Examples__
-- >>> permutationsort [16, 23, 4, 8, 15, 42]
-- [4,8,15,16,23,42]
--
-- >>> permutationsort [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)]
-- [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
permutationsort :: (Ord a) => [a] -> [a]
permutationsort xs = acc (permutations xs) []
  where
    acc :: (Ord a) => [[a]] -> [a] -> [a]
    acc [] unsortedPermutations =
      permutationsort unsortedPermutations
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted permutation = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
