{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Robustsort.Bytesort (bytesort4Bit)
import Data.Robustsort.OtherSorts.Mergesort (mergesort)
import Data.Robustsort.OtherSorts.Quicksort (quicksort)
import Data.Robustsort.Subalgorithms.Bogosort (bogosort)
import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Subalgorithms.Permutationsort (permutationsort)
import Data.Robustsort.Utils.Types (Sortable (..), fromSortInt)

unsortedArray :: [Int]
unsortedArray = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (fromSortInt (bubblesort (SortInt unsortedArray))))
  putStrLn ("mergesort: " ++ show (mergesort unsortedArray))
  putStrLn ("quicksort: " ++ show (quicksort unsortedArray))
  putStrLn ("permutationsort: " ++ show (permutationsort [3, 1, 2]))
  putStrLn ("bogosort: " ++ show (bogosort [3, 1, 2] 143))
  putStrLn ("bytesort4Bit: " ++ show (bytesort4Bit unsortedArray))
