module Data.Tensort.Subalgorithms.Permutationsort (permutationsort) where

import Data.List (permutations)
import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types (Record, Sortable (..), fromSortInt, fromSortRec)

permutationsort :: Sortable -> Sortable
permutationsort (SortInt xs) = SortInt (acc (permutations x) [])
  where
    x = xs
    acc :: [[Int]] -> [Int] -> [Int]
    acc [] unsortedPermutations = fromSortInt (permutationsort (SortInt unsortedPermutations))
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted (SortInt permutation) = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
permutationsort (SortRec xs) = SortRec (acc (permutations x) [])
  where
    x = xs
    acc :: [[Record]] -> [Record] -> [Record]
    acc [] unsortedPermutations = fromSortRec (permutationsort (SortRec unsortedPermutations))
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted (SortRec permutation) = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
