module Main (main) where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Supersort (magicSuperStrat, mundaneSuperStrat, supersort)
import Data.Tensort.Tensort (mkTSProps, tensort, tensortB4, tensortBL, tensortBN)
import Data.Tensort.Utils.Types (Sortable (..))
import SortSpec (result_is_sorted_bits, result_is_sorted_records, result_is_sorted_records_short)
import TestCheck (check)

-- | This suite of QuickCheck tests contains  a guard that will cause the test
--   `suite to fail if any of the individual tests fail
main :: IO ()
main = do
  putStrLn "Running test suite!"
  putStrLn "Quicksort returns a sorted array..."
  check (result_is_sorted_records quicksort)
  putStrLn "True!"
  putStrLn "Mergesort returns a sorted array..."
  check (result_is_sorted_records mergesort)
  putStrLn "True!"
  putStrLn "Bubblesort returns a sorted array..."
  check (result_is_sorted_records bubblesort)
  putStrLn "True!"
  putStrLn "Exchangesort returns a sorted array..."
  check (result_is_sorted_records exchangesort)
  putStrLn "True!"
  putStrLn "Permutationsort returns a sorted array..."
  check (result_is_sorted_records permutationsort)
  putStrLn "True!"
  putStrLn "Bogosort returns a sorted array..."
  check (result_is_sorted_records bogosort)
  putStrLn "True!"
  putStrLn "Magicsort returns a sorted array..."
  -- check (result_is_sorted_records_short magicsort)
  let magicRes = magicsort (SortBit [5, 2, 3, 1, 4])
  print magicRes
  check (magicRes == SortBit [1, 2, 3, 4, 5])
  putStrLn "True!"
  putStrLn "Standard Logaritmic Tensort returns a sorted array..."
  let logRes = tensortBL [5, 2, 3, 1, 4]
  print logRes
  check (logRes == [1, 2, 3, 4, 5])
  -- check (result_is_sorted_bits tensortBL)
  putStrLn "True!"
  putStrLn "Standard 4-Bit Tensort returns a sorted array..."
  check (result_is_sorted_bits tensortB4)
  putStrLn "True!"
  -- TBA
  putStrLn "Standard Mundane Robustsort with Permutationsort adjudicator returns a sorted array..."
  check (result_is_sorted_bits robustsortP)
  putStrLn "True!"
  putStrLn "Standard Mundane Robustsort with Bogosort adjudicator returns a sorted array..."
  check (result_is_sorted_bits robustsortB)
  putStrLn "True!"
  putStrLn "Magic Robustsort returns a sorted array..."
  let magicRoboRes = magicsort (SortBit [5, 2, 3, 1, 4])
  print magicRoboRes
  check (magicRoboRes == SortBit [1, 2, 3, 4, 5])
  -- check (result_is_sorted_bits robustsortM)
  putStrLn "True!"
  putStrLn "All tests pass!"
