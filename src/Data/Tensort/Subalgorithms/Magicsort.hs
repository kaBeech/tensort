-- | This module provides the magicsort function for sorting Sortable lists
module Data.Tensort.Subalgorithms.Magicsort
  ( magicsort,
  )
where

import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Utils.Types (Sortable (..))

-- | Takes a Sortable and returns a sorted Sortable.
--
--   Runs both Permutationsort and Bogosort on the input Sortable and compares
--   the results. If the results agree, returns the result of Permutationsort,
--   otherwise repeats the process.

-- | ==== __Examples__
-- >>> magicsort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> magicsort (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15), (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
magicsort :: Sortable -> Sortable
magicsort xs =
  if verifyResults result1 result2
    then result1
    else magicsort xs
  where
    result1 = permutationsort xs
    result2 = bogosort xs

verifyResults :: Sortable -> Sortable -> Bool
verifyResults (SortBit xs) (SortBit ys) = xs == ys
verifyResults (SortRec xs) (SortRec ys) = map snd xs == map snd ys
verifyResults (SortBit _) (SortRec _) = False
verifyResults (SortRec _) (SortBit _) = False
