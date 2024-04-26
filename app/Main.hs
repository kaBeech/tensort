module Main where

import Bubblesort qualified (bubblesort)
import CoinFlip qualified (flipTenCoins)
import Mergesort qualified (mergesort)
import Quicksort qualified (quicksort)
import Wonky qualified (trinaryCompare)

unsortedArray :: [String]
unsortedArray = ["b", "e", "j", "d", "o", "k", "g", "n", "p", "f", "m", "c", "h", "i", "l", "a"]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ unwords (Bubblesort.bubblesort unsortedArray))
  putStrLn ("mergesort: " ++ unwords (Mergesort.mergesort unsortedArray))
  putStrLn ("quicksort: " ++ unwords (Quicksort.quicksort unsortedArray))
  putStrLn ("Flip Ten Coins: " ++ show (CoinFlip.flipTenCoins 3343))
  putStrLn ("Trinary Compare: " ++ show (Wonky.trinaryCompare 1 2 50 345))
