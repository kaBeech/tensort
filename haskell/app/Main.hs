module Main where

import Bubblesort qualified (bubblesort)
import Dice qualified (randomBoolList1021)
import Mergesort qualified (mergesort)
import Pseudorandom qualified (randomizeArray)
import Quicksort qualified (quicksort)

unsortedArray :: [Int]
unsortedArray = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (Bubblesort.bubblesort unsortedArray))
  putStrLn ("mergesort: " ++ show (Mergesort.mergesort unsortedArray))
  putStrLn ("quicksort: " ++ show (Quicksort.quicksort unsortedArray))
  putStrLn ("randomizeArray: " ++ show (Pseudorandom.randomizeArray [1 .. 16]))
  let randomizedArray = Pseudorandom.randomizeArray [1 .. 1024]
  putStrLn ("randomizeArray 1021: " ++ show randomizedArray)
  writeFile "generated_data/randomized_array.dat" (show randomizedArray)
  writeFile "generated_data/bool_table.dat" (show Dice.randomBoolList1021)
