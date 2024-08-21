module Main (main) where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP, robustsortRB, robustsortRM, robustsortRP)
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Supersort (magicSuperStrat, mundaneSuperStrat, supersort)
import Data.Tensort.Tensort (tensort, tensortB4, tensortBL)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.Types (Sortable)
import SortSpec (result_is_sorted_bits, result_is_sorted_custom_bitsize, result_is_sorted_records, result_is_sorted_records_short)
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
  check (result_is_sorted_records_short bogosort)
  putStrLn "True!"
  putStrLn "Magicsort returns a sorted array..."
  check (result_is_sorted_records_short magicsort)
  putStrLn "True!"
  putStrLn "Standard Logaritmic Tensort returns a sorted array..."
  check (result_is_sorted_bits tensortBL)
  putStrLn "True!"
  putStrLn "Standard 4-Bit Tensort returns a sorted array..."
  check (result_is_sorted_bits tensortB4)
  putStrLn "True!"
  putStrLn "Standard Custom Bitsize Tensort returns a sorted array..."
  check result_is_sorted_custom_bitsize
  putStrLn "True!"
  putStrLn
    "Standard Mundane Robustsort with Permutationsort adjudicator returns a \
    \sorted array..."
  check (result_is_sorted_bits robustsortP)
  putStrLn "True!"
  putStrLn
    "Standard Mundane Robustsort with Bogosort adjudicator returns a sorted \
    \array..."
  check (result_is_sorted_bits robustsortB)
  putStrLn "True!"
  putStrLn "Magic Robustsort returns a sorted array..."
  check (result_is_sorted_bits robustsortM)
  putStrLn "True!"
  putStrLn "Custom Tensort returns a sorted array..."
  check (result_is_sorted_bits tensortCustomExample)
  putStrLn "True!"
  putStrLn "Custom Mundane Supersort returns a sorted array..."
  check (result_is_sorted_records_short supersortMundaneCustomExample)
  putStrLn "True!"
  putStrLn "Custom Magic Supersort returns a sorted array..."
  check (result_is_sorted_records_short supersortMagicCustomExample)
  putStrLn "True!"
  putStrLn "Custom Mundane Robustsort returns a sorted array..."
  check (result_is_sorted_bits robustsortMundaneCustomExample)
  putStrLn "True!"
  putStrLn "Custom Magic Robustsort returns a sorted array..."
  check (result_is_sorted_bits robustsortMagicCustomExample)
  putStrLn "True!"
  putStrLn
    "Recursive Mundane Robustsort with Permutationsort adjudicator \
    \returns a sorted array..."
  check (result_is_sorted_bits robustsortRP)
  putStrLn "True!"
  putStrLn
    "Recursive Mundane Robustsort with Bogosort adjudicator returns \
    \a sorted array..."
  check (result_is_sorted_bits robustsortRB)
  putStrLn "True!"
  putStrLn "Recursive Magic Robustsort returns a sorted array..."
  check (result_is_sorted_bits robustsortRM)
  putStrLn "True!"
  putStrLn "All tests pass!"

tensortCustomExample :: Sortable -> Sortable
tensortCustomExample = tensort (mkTsProps 8 mergesort)

supersortMundaneCustomExample :: Sortable -> Sortable
supersortMundaneCustomExample =
  supersort
    ( quicksort,
      magicsort,
      bubblesort,
      mundaneSuperStrat
    )

supersortMagicCustomExample :: Sortable -> Sortable
supersortMagicCustomExample =
  supersort
    ( bogosort,
      permutationsort,
      magicsort,
      magicSuperStrat
    )

robustsortMundaneCustomExample :: Sortable -> Sortable
robustsortMundaneCustomExample =
  tensort
    (mkTsProps 3 supersortMundaneCustomExample)

robustsortMagicCustomExample :: Sortable -> Sortable
robustsortMagicCustomExample =
  tensort
    (mkTsProps 3 supersortMagicCustomExample)
