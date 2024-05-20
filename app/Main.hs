{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Robustsort.Bubblesort qualified (bubblesort)
import Robustsort.Mergesort qualified (mergesort)
import Robustsort.Quicksort qualified (quicksort)

unsortedArray :: [Int]
unsortedArray = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (Robustsort.Bubblesort.bubblesort unsortedArray))
  putStrLn ("mergesort: " ++ show (Robustsort.Mergesort.mergesort unsortedArray))
  putStrLn ("quicksort: " ++ show (Robustsort.Quicksort.quicksort unsortedArray))
