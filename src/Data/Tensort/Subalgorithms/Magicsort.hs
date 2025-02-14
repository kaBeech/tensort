-- | This module provides the magicsort function for sorting lists
module Data.Tensort.Subalgorithms.Magicsort
  ( magicsort,
  )
where

import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)

-- | Takes a list and returns a sorted list.
--
--   Runs both Permutationsort and Bogosort on the input list and compares
--   the results. If the results agree, returns the result of Permutationsort,
--   otherwise repeats the process.

-- | ==== __Examples__
-- >>> magicsort [16, 23, 4, 8, 15, 42]
-- SortBit [4,8,15,16,23,42]
--
-- >>> magicsort [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15), (4, 42)]
-- [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
magicsort :: (Ord a) => [a] -> [a]
magicsort xs =
  if result1 == result2
    then result1
    else magicsort xs
  where
    result1 = permutationsort xs
    result2 = bogosort xs
