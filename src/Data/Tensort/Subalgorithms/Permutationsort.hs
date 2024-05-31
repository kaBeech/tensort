module Data.Tensort.Subalgorithms.Permutationsort (permutationsort) where

import Data.List (permutations)
import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types (Record, Sortable (..), fromSortBit, fromSortRec, Bit)

permutationsort :: Sortable -> Sortable
permutationsort (SortBit xs) = SortBit (acc (permutations x) [])
  where
    x = xs
    acc :: [[Bit]] -> [Bit] -> [Bit]
    acc [] unsortedPermutations = fromSortBit (permutationsort (SortBit unsortedPermutations))
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted (SortBit permutation) = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
permutationsort (SortRec xs) = SortRec (acc (permutations x) [])
  where
    x = xs
    acc :: [[Record]] -> [Record] -> [Record]
    acc [] unsortedPermutations = fromSortRec (permutationsort (SortRec unsortedPermutations))
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted (SortRec permutation) = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
