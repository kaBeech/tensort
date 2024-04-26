module Main where

import Bubblesort qualified (bubblesort)
import CoinFlip qualified (flipTenCoins)
import Mergesort qualified (mergesort)
import Quicksort qualified (quicksort)
import Wonky qualified (trinaryCompare)

unsortedArray :: [Int]
unsortedArray = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (Bubblesort.bubblesort unsortedArray))
  putStrLn ("mergesort: " ++ show (Mergesort.mergesort unsortedArray))
  putStrLn ("quicksort: " ++ show (Quicksort.quicksort unsortedArray))
  putStrLn ("Flip Ten Coins: " ++ show (CoinFlip.flipTenCoins 3343))
  putStrLn ("Trinary Compare: " ++ show (Wonky.trinaryCompare 1 2 50 345))
