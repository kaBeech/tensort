{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main (main) where

import Test.QCheck
import Test.SortSpec (result_is_sorted_custom_bitsize_ints)
import Test.SortingAlgorithms
import Test.TestCheck (check)

-- | This suite of QuickCheck tests contains  a guard that will cause the test
--   suite to fail if any of the individual tests fail
main :: IO ()
main = do
  mapM_ qcheckBitsInt sortingAlgorithmsSortable
  mapM_ qcheckRecsInt sortingAlgorithmsSortable
  mapM_ qcheckRecsShortInt sortingAlgorithmsSortableShort
  mapM_ qcheckRecsTinyInt sortingAlgorithmsSortableTiny
  mapM_ qcheckBitsOnlyInt sortingAlgorithmsBits
  putStrLn "Running test suite!"
  putStrLn "Standard Custom Bitsize Tensort returns a sorted array..."
  check result_is_sorted_custom_bitsize_ints
  putStrLn "True!"
  putStrLn "All tests pass!"
