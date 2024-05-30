module Main where

import Data.Tensort.Tensort (tensort4Bit)
import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.ReverseExchangesort (reverseExchangesort)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (SortAlg, Sortable (..), fromSortInt)
import Data.Time.Clock

unsortedInts :: [Int]
unsortedInts = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

unsortedInts52 :: Sortable
unsortedInts52 = randomizeList (SortInt [1 .. 52]) 143

unsortedInts1000 :: Sortable
unsortedInts1000 = randomizeList (SortInt [1 .. 1000]) 143

unsortedInts10000 :: Sortable
unsortedInts10000 = randomizeList (SortInt [1 .. 10000]) 143

unsortedInts100000 :: Sortable
unsortedInts100000 = randomizeList (SortInt [1 .. 100000]) 143

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bubblesort: " ++ show (fromSortInt (bubblesort (SortInt unsortedInts))))
  putStrLn ("reverseExchangesort: " ++ show (fromSortInt (reverseExchangesort (SortInt unsortedInts))))
  putStrLn ("mergesort: " ++ show (mergesort (SortInt unsortedInts)))
  putStrLn ("quicksort: " ++ show (quicksort (SortInt unsortedInts)))
  putStrLn ("permutationsort: " ++ show (permutationsort (SortInt [3, 1, 2])))
  putStrLn ("bogosort: " ++ show (bogosort (SortInt [3, 1, 2])))
  putStrLn ("magicsort: " ++ show (magicsort (SortInt [3, 1, 2])))
  putStrLn ("tensort4Bit: " ++ show (tensort4Bit (SortInt unsortedInts)))
  putStrLn ("robustsortP: " ++ show (robustsortP (SortInt unsortedInts)))
  putStrLn ("robustsortB: " ++ show (robustsortB (SortInt unsortedInts)))
  putStrLn ("robustsortM: " ++ show (robustsortM (SortInt unsortedInts)))
  printTime unsortedInts52
  printTime unsortedInts1000
  printTime unsortedInts10000
  printTime unsortedInts100000

printTime :: Sortable -> IO ()
printTime l = do
  putStr (" Algorithm   | Time         | n =")
  startRSortP <- getCurrentTime
  putStrLn (" " ++ (show (length (fromSortInt (robustsortP l)))))
  endRSortP <- getCurrentTime
  putStr (" RobustsortP | " ++ show (diffUTCTime endRSortP startRSortP) ++ " | ")
  startRSortB <- getCurrentTime
  putStrLn ("    " ++ (show (length (fromSortInt (robustsortB l)))))
  endRSortB <- getCurrentTime
  putStr (" RobustsortB | " ++ show (diffUTCTime endRSortB startRSortB) ++ " | ")
  startRSortM <- getCurrentTime
  putStrLn ("    " ++ (show (length (fromSortInt (robustsortM l)))))
  endRSortM <- getCurrentTime
  putStr (" RobustsortM | " ++ show (diffUTCTime endRSortM startRSortM) ++ " | ")
  startMergesort <- getCurrentTime
  putStrLn ("    " ++ (show (length (fromSortInt (mergesort l)))))
  endMergesort <- getCurrentTime
  putStr (" Mergesort   | " ++ show (diffUTCTime endMergesort startMergesort) ++ " | ")
  startQuicksort <- getCurrentTime
  putStrLn ("    " ++ (show (length (fromSortInt (quicksort l)))))
  endQuicksort <- getCurrentTime
  putStr (" Quicksort   | " ++ show (diffUTCTime endQuicksort startQuicksort) ++ " | ")
  startBubblesort <- getCurrentTime
  putStrLn ("    " ++ (show (length (fromSortInt (bubblesort l)))))
  endBubblesort <- getCurrentTime
  putStr (" Bubblesort  | " ++ show (diffUTCTime endBubblesort startBubblesort) ++ " | ")
  putStrLn ("    " ++ (show (length (fromSortInt (bubblesort l)))))
  putStrLn ("----------------------------------------------------------")
