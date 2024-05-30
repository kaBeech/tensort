module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortBasic2Bit, tensortBasic3Bit, tensortBasic4Bit)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), fromSortBit)
import Data.Time.Clock

unsortedBits :: [Int]
unsortedBits = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

unsortedBits52 :: Sortable
unsortedBits52 = randomizeList (SortBit [1 .. 52]) 143

unsortedBits1000 :: Sortable
unsortedBits1000 = randomizeList (SortBit [1 .. 1000]) 143

unsortedBits10000 :: Sortable
unsortedBits10000 = randomizeList (SortBit [1 .. 10000]) 143

unsortedBits100000 :: Sortable
unsortedBits100000 = randomizeList (SortBit [1 .. 100000]) 143

main :: IO ()
main = do
  printTime unsortedBits52
  printTime unsortedBits1000
  printTime unsortedBits10000
  printTime unsortedBits100000

printTime :: Sortable -> IO ()
printTime l = do
  putStr " Algorithm   | Time         | n ="
  startTensort2Bit <- getCurrentTime
  putStrLn (" " ++ show (length (tensortBasic2Bit (fromSortBit l))))
  endTensort2Bit <- getCurrentTime
  putStr (" Tensort2Bit | " ++ show (diffUTCTime endTensort2Bit startTensort2Bit) ++ " | ")
  startTensort3Bit <- getCurrentTime
  putStrLn ("    " ++ show (length (tensortBasic3Bit (fromSortBit l))))
  endTensort3Bit <- getCurrentTime
  putStr (" Tensort3Bit | " ++ show (diffUTCTime endTensort3Bit startTensort3Bit) ++ " | ")
  startTensort4Bit <- getCurrentTime
  putStrLn ("    " ++ show (length (tensortBasic4Bit (fromSortBit l))))
  endTensort4Bit <- getCurrentTime
  putStr (" Tensort4Bit | " ++ show (diffUTCTime endTensort4Bit startTensort4Bit) ++ " | ")
  startRSortP <- getCurrentTime
  putStrLn ("    " ++ show (length (robustsortP (fromSortBit l))))
  endRSortP <- getCurrentTime
  putStr (" RobustsortP | " ++ show (diffUTCTime endRSortP startRSortP) ++ " | ")
  startRSortB <- getCurrentTime
  putStrLn ("    " ++ show (length (robustsortB (fromSortBit l))))
  endRSortB <- getCurrentTime
  putStr (" RobustsortB | " ++ show (diffUTCTime endRSortB startRSortB) ++ " | ")
  startRSortM <- getCurrentTime
  putStrLn ("    " ++ show (length (robustsortM (fromSortBit l))))
  endRSortM <- getCurrentTime
  putStr (" RobustsortM | " ++ show (diffUTCTime endRSortM startRSortM) ++ " | ")
  startMergesort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (mergesort l))))
  endMergesort <- getCurrentTime
  putStr (" Mergesort   | " ++ show (diffUTCTime endMergesort startMergesort) ++ " | ")
  startQuicksort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (quicksort l))))
  endQuicksort <- getCurrentTime
  putStr (" Quicksort   | " ++ show (diffUTCTime endQuicksort startQuicksort) ++ " | ")
  startBubblesort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (bubblesort l))))
  endBubblesort <- getCurrentTime
  putStr (" Bubblesort  | " ++ show (diffUTCTime endBubblesort startBubblesort) ++ " | ")
  putStrLn ("    " ++ show (length (fromSortBit (bubblesort l))))
  putStrLn "----------------------------------------------------------"
