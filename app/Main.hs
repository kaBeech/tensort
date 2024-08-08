module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP, robustsortRM, robustsortRecursive)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortB4, tensortBL)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), fromSortBit)
import Data.Time.Clock

genUnsortedBits :: Int -> Sortable
genUnsortedBits n = randomizeList 143 (SortBit [1 .. n])

main :: IO ()
main = do
  -- Eventually I hope to turn that 14 into a 20
  printTimes (map (genUnsortedBits . (2 ^)) [3 .. 14])

printTimes :: [Sortable] -> IO ()
printTimes [] = return ()
printTimes (x : xs) = do
  printTime x
  printTimes xs

printTime :: Sortable -> IO ()
printTime l = do
  putStr " Algorithm   | Time         | n ="
  startTensortB4 <- getCurrentTime
  putStrLn (" " ++ show (length (fromSortBit (tensortB4 l))))
  endTensortB4 <- getCurrentTime
  putStr (" Tensort4Bit | " ++ show (diffUTCTime endTensortB4 startTensortB4) ++ " | ")
  startTensortBL <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (tensortBL l))))
  endTensortBL <- getCurrentTime
  putStr (" TensortBL   | " ++ show (diffUTCTime endTensortBL startTensortBL) ++ " | ")
  startRSortP <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (robustsortP l))))
  endRSortP <- getCurrentTime
  putStr (" RobustsortP | " ++ show (diffUTCTime endRSortP startRSortP) ++ " | ")
  startRSortB <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (robustsortB l))))
  endRSortB <- getCurrentTime
  putStr (" RobustsortB | " ++ show (diffUTCTime endRSortB startRSortB) ++ " | ")
  startRSortM <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (robustsortM l))))
  endRSortM <- getCurrentTime
  putStr (" RobustsortM | " ++ show (diffUTCTime endRSortM startRSortM) ++ " | ")
  startRSortRM <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (robustsortRM l))))
  endRSortRM <- getCurrentTime
  putStr (" RobustsortRM | " ++ show (diffUTCTime endRSortRM startRSortRM) ++ " | ")
  startMergesort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (mergesort l))))
  endMergesort <- getCurrentTime
  putStr (" Mergesort   | " ++ show (diffUTCTime endMergesort startMergesort) ++ " | ")
  startQuicksort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (quicksort l))))
  endQuicksort <- getCurrentTime
  putStr (" Quicksort   | " ++ show (diffUTCTime endQuicksort startQuicksort) ++ " | ")
  startBubblesort <- getCurrentTime
  putStrLn ("     " ++ show (length (fromSortBit (bubblesort l))))
  endBubblesort <- getCurrentTime
  putStr (" Bubblesort  | " ++ show (diffUTCTime endBubblesort startBubblesort) ++ " | ")
  putStrLn ("    " ++ show (length (fromSortBit l)))
  putStrLn "----------------------------------------------------------"
