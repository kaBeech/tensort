{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Robustsort.Bogosort qualified (bogosort)
import Robustsort.Bubblesort qualified (bubblesort)
import Robustsort.Mergesort qualified (mergesort)
import Robustsort.Permutationsort qualified (permutationsort)
import Robustsort.Quicksort qualified (quicksort)

unsortedArray :: [Int]
unsortedArray = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (Robustsort.Bubblesort.bubblesort unsortedArray))
  putStrLn ("mergesort: " ++ show (Robustsort.Mergesort.mergesort unsortedArray))
  putStrLn ("quicksort: " ++ show (Robustsort.Quicksort.quicksort unsortedArray))
  putStrLn ("permutationsort: " ++ show (Robustsort.Permutationsort.permutationsort [3, 1, 2]))
  putStrLn ("bogosort: " ++ show (Robustsort.Bogosort.bogosort [3, 1, 2] 143))
