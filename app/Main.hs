module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortBasic2Bit, tensortBasic3Bit, tensortBasic4Bit)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), fromSortInt)
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
  printTime unsortedInts52
  printTime unsortedInts1000
  printTime unsortedInts10000
  printTime unsortedInts100000

printTime :: Sortable -> IO ()
printTime l = do
  putStr " Algorithm   | Time         | n ="
  startTensort2Bit <- getCurrentTime
  putStrLn (" " ++ show (length (tensortBasic2Bit (fromSortInt l))))
  endTensort2Bit <- getCurrentTime
  putStr (" Tensort2Bit | " ++ show (diffUTCTime endTensort2Bit startTensort2Bit) ++ " | ")
  startTensort3Bit <- getCurrentTime
  putStrLn ("    " ++ show (length (tensortBasic3Bit (fromSortInt l))))
  endTensort3Bit <- getCurrentTime
  putStr (" Tensort3Bit | " ++ show (diffUTCTime endTensort3Bit startTensort3Bit) ++ " | ")
  startTensort4Bit <- getCurrentTime
  putStrLn ("    " ++ show (length (tensortBasic4Bit (fromSortInt l))))
  endTensort4Bit <- getCurrentTime
  putStr (" Tensort4Bit | " ++ show (diffUTCTime endTensort4Bit startTensort4Bit) ++ " | ")
  startRSortP <- getCurrentTime
  putStrLn ("    " ++ show (length (robustsortP (fromSortInt l))))
  endRSortP <- getCurrentTime
  putStr (" RobustsortP | " ++ show (diffUTCTime endRSortP startRSortP) ++ " | ")
  startRSortB <- getCurrentTime
  putStrLn ("    " ++ show (length (robustsortB (fromSortInt l))))
  endRSortB <- getCurrentTime
  putStr (" RobustsortB | " ++ show (diffUTCTime endRSortB startRSortB) ++ " | ")
  startRSortM <- getCurrentTime
  putStrLn ("    " ++ show (length (robustsortM (fromSortInt l))))
  endRSortM <- getCurrentTime
  putStr (" RobustsortM | " ++ show (diffUTCTime endRSortM startRSortM) ++ " | ")
  startMergesort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortInt (mergesort l))))
  endMergesort <- getCurrentTime
  putStr (" Mergesort   | " ++ show (diffUTCTime endMergesort startMergesort) ++ " | ")
  startQuicksort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortInt (quicksort l))))
  endQuicksort <- getCurrentTime
  putStr (" Quicksort   | " ++ show (diffUTCTime endQuicksort startQuicksort) ++ " | ")
  startBubblesort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortInt (bubblesort l))))
  endBubblesort <- getCurrentTime
  putStr (" Bubblesort  | " ++ show (diffUTCTime endBubblesort startBubblesort) ++ " | ")
  putStrLn ("    " ++ show (length (fromSortInt (bubblesort l))))
  putStrLn "----------------------------------------------------------"
