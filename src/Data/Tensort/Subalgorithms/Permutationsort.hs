-- | This module provides the permutationsort function for sorting lists using the
--   Sortable type
module Data.Tensort.Subalgorithms.Permutationsort (permutationsort) where

import Data.List (permutations)
import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types
  ( Bit,
    Record,
    Sortable (..),
    fromSortBit,
    fromSortRec,
  )

-- | Takes a Sortable and returns a sorted Sortable using a Permutationsort
--   algorithm

-- | ==== __Examples__
-- >>> permutationsort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> permutationsort (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
permutationsort :: Sortable -> Sortable
permutationsort (SortBit xs) = SortBit (acc (permutations x) [])
  where
    x = xs
    acc :: [[Bit]] -> [Bit] -> [Bit]
    acc [] unsortedPermutations =
      fromSortBit (permutationsort (SortBit unsortedPermutations))
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted (SortBit permutation) = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
permutationsort (SortRec xs) = SortRec (acc (permutations x) [])
  where
    x = xs
    acc :: [[Record]] -> [Record] -> [Record]
    acc [] unsortedPermutations =
      fromSortRec (permutationsort (SortRec unsortedPermutations))
    acc (permutation : remainingPermutations) unsortedPermutations
      | isSorted (SortRec permutation) = permutation
      | otherwise = acc remainingPermutations unsortedPermutations
