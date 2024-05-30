module Main where

import Data.Robustsort.Bytesort (bytesort4Bit)
import Data.Robustsort.OtherSorts.Mergesort (mergesort)
import Data.Robustsort.OtherSorts.Quicksort (quicksort)
import Data.Robustsort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Robustsort.Subalgorithms.Bogosort (bogosort)
import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Subalgorithms.Magicsort (magicsort)
import Data.Robustsort.Subalgorithms.Permutationsort (permutationsort)
import Data.Robustsort.Subalgorithms.ReverseExchangesort (reverseExchangesort)
import Data.Robustsort.Utils.Types (Sortable (..), fromSortInt)

unsortedArray :: [Int]
unsortedArray = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (fromSortInt (bubblesort (SortInt unsortedArray))))
  putStrLn ("reverseExchangesort: " ++ show (fromSortInt (reverseExchangesort (SortInt unsortedArray))))
  putStrLn ("mergesort: " ++ show (mergesort (SortInt unsortedArray)))
  putStrLn ("quicksort: " ++ show (quicksort (SortInt unsortedArray)))
  putStrLn ("permutationsort: " ++ show (permutationsort (SortInt [3, 1, 2])))
  putStrLn ("bogosort: " ++ show (bogosort (SortInt [3, 1, 2])))
  putStrLn ("magicsort: " ++ show (magicsort (SortInt [3, 1, 2])))
  putStrLn ("bytesort4Bit: " ++ show (bytesort4Bit (SortInt unsortedArray)))
  putStrLn ("robustsortP: " ++ show (robustsortP (SortInt unsortedArray)))
  putStrLn ("robustsortB: " ++ show (robustsortB (SortInt unsortedArray)))
  putStrLn ("robustsortM: " ++ show (robustsortM (SortInt unsortedArray)))
