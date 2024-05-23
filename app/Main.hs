{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Robustsort.OtherSorts.Mergesort qualified (mergesort)
import Data.Robustsort.OtherSorts.Quicksort qualified (quicksort)
import Data.Robustsort.Subalgorithms.Bogosort qualified (bogosort)
import Data.Robustsort.Subalgorithms.Bubblesort qualified (bubblesort)
import Data.Robustsort.Subalgorithms.Permutationsort qualified (permutationsort)

unsortedArray :: [Int]
unsortedArray = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (Data.Robustsort.Subalgorithms.Bubblesort.bubblesort unsortedArray))
  putStrLn ("mergesort: " ++ show (Data.Robustsort.OtherSorts.Mergesort.mergesort unsortedArray))
  putStrLn ("quicksort: " ++ show (Data.Robustsort.OtherSorts.Quicksort.quicksort unsortedArray))
  putStrLn ("permutationsort: " ++ show (Data.Robustsort.Subalgorithms.Permutationsort.permutationsort [3, 1, 2]))
  putStrLn ("bogosort: " ++ show (Data.Robustsort.Subalgorithms.Bogosort.bogosort [3, 1, 2] 143))
