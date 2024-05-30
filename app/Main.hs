module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortB4, tensortBL)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), fromSortBit)
import Data.Time.Clock

unsortedBits :: [Int]
unsortedBits = [2, 5, 10, 4, 15, 11, 7, 14, 16, 6, 13, 3, 8, 9, 12, 1]

genUnsortedBits :: Int -> Sortable
genUnsortedBits n = randomizeList (SortBit [1 .. n]) 143

main :: IO ()
main = do
  printTimes (map genUnsortedBits [52, 1000, 10000, 50000, 100000])

printTimes :: [Sortable] -> IO ()
printTimes [] = return ()
printTimes (x : xs) = do
  printTime x
  printTimes xs

printTime :: Sortable -> IO ()
printTime l = do
  putStr " Algorithm   | Time         | n ="
  startTensortB4 <- getCurrentTime
  putStrLn (" " ++ show (length (tensortB4 (fromSortBit l))))
  endTensortB4 <- getCurrentTime
  putStr (" Tensort4Bit | " ++ show (diffUTCTime endTensortB4 startTensortB4) ++ " | ")
  startTensortBL <- getCurrentTime
  putStrLn ("    " ++ show (length (tensortBL (fromSortBit l))))
  endTensortBL <- getCurrentTime
  putStr (" tensortBL   | " ++ show (diffUTCTime endTensortBL startTensortBL) ++ " | ")
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
