module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortB4, tensortBL)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState (..), fromSortBit)
import Data.Time.Clock
import System.Random (mkStdGen)

genUnsortedBits :: Int -> Sortable
genUnsortedBits n = randomizeList (SortBit [1 .. n]) 143

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
  let wonkySt = WonkyState {wonkyChance = 10, stuckChance = 10, previousAnswer = 0, stdGen = mkStdGen 143}
  putStr " Algorithm   | Time         | n ="
  startTensortB4 <- getCurrentTime
  putStrLn (" " ++ show (length (fst (tensortB4 (fromSortBit l) wonkySt))))
  endTensortB4 <- getCurrentTime
  putStr (" Tensort4Bit | " ++ show (diffUTCTime endTensortB4 startTensortB4) ++ " | ")
  startTensortBL <- getCurrentTime
  putStrLn ("    " ++ show (length (fst (tensortBL (fromSortBit l) wonkySt))))
  endTensortBL <- getCurrentTime
  putStr (" TensortBL   | " ++ show (diffUTCTime endTensortBL startTensortBL) ++ " | ")
  startRSortP <- getCurrentTime
  putStrLn ("    " ++ show (length (fst (robustsortP (fromSortBit l) wonkySt))))
  endRSortP <- getCurrentTime
  putStr (" RobustsortP | " ++ show (diffUTCTime endRSortP startRSortP) ++ " | ")
  startRSortB <- getCurrentTime
  putStrLn ("    " ++ show (length (fst (robustsortB (fromSortBit l) wonkySt))))
  endRSortB <- getCurrentTime
  putStr (" RobustsortB | " ++ show (diffUTCTime endRSortB startRSortB) ++ " | ")
  startRSortM <- getCurrentTime
  putStrLn ("    " ++ show (length (fst (robustsortM (fromSortBit l) wonkySt))))
  endRSortM <- getCurrentTime
  putStr (" RobustsortM | " ++ show (diffUTCTime endRSortM startRSortM) ++ " | ")
  startMergesort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (fst (mergesort l wonkySt)))))
  endMergesort <- getCurrentTime
  putStr (" Mergesort   | " ++ show (diffUTCTime endMergesort startMergesort) ++ " | ")
  startQuicksort <- getCurrentTime
  putStrLn ("    " ++ show (length (fromSortBit (fst (quicksort l wonkySt)))))
  endQuicksort <- getCurrentTime
  putStr (" Quicksort   | " ++ show (diffUTCTime endQuicksort startQuicksort) ++ " | ")
  startBubblesort <- getCurrentTime
  putStrLn ("     " ++ show (length (fromSortBit (fst (bubblesort l wonkySt)))))
  endBubblesort <- getCurrentTime
  putStr (" Bubblesort  | " ++ show (diffUTCTime endBubblesort startBubblesort) ++ " | ")
  putStrLn ("    " ++ show (length (fromSortBit l)))
  putStrLn "----------------------------------------------------------"
