module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort (robustsortB, robustsortM, robustsortP)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortB4, tensortBL)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Score (getTotalPositionalErrors)
import Data.Tensort.Utils.Types (Bit, SortAlg, Sortable (..), WonkyState (..), fromSortBit)
import Data.Time.Clock
import System.Random (mkStdGen)

genUnsortedBits :: Int -> Sortable
genUnsortedBits n = randomizeList (SortBit [1 .. n]) 143

genTestPeriod :: Int -> Int
genTestPeriod n = 2 ^ n

padOut :: String -> Int -> String
padOut s totalLength = s ++ replicate (totalLength - length s) ' '

-- This is to force times to record before and after the sort, otherwise
-- the functions run asynchronously
getCurrentTimeArg :: Int -> IO UTCTime
getCurrentTimeArg i = do
  let time = i * 0
  putStr (replicate time ' ')
  getCurrentTime

printResultSortable :: String -> Sortable -> SortAlg -> WonkyState -> IO ()
printResultSortable sortName l sortAlg wonkySt = do
  startTime <- getCurrentTime
  let result = fst (sortAlg l wonkySt)
  endTime <- getCurrentTimeArg (length (fromSortBit result))
  composeResultString sortName startTime endTime result

printResultBits :: String -> Sortable -> ([Bit] -> WonkyState -> ([Bit], WonkyState)) -> WonkyState -> IO ()
printResultBits sortName l sortAlg wonkySt = do
  startTime <- getCurrentTime
  let result = fst (sortAlg (fromSortBit l) wonkySt)
  endTime <- getCurrentTimeArg (head result)
  composeResultString sortName startTime endTime (SortBit result)

composeResultString :: String -> UTCTime -> UTCTime -> Sortable -> IO ()
composeResultString sortName startTime endTime result = do
  let nameString = padOut sortName 12
  let timeString = padOut (show (diffUTCTime endTime startTime)) 15
  let scoreString = padOut (show (getTotalPositionalErrors (fromSortBit result))) 8
  putStrLn
    ( " "
        ++ nameString
        ++ " | "
        ++ timeString
        ++ " | "
        ++ scoreString
        ++ " | "
    )

main :: IO ()
main = do
  -- Eventually I hope to turn that 14 into a 20
  printTimes (map (genUnsortedBits . genTestPeriod) [3 .. 14])

printTimes :: [Sortable] -> IO ()
printTimes [] = return ()
printTimes (x : xs) = do
  printTime x
  printTimes xs

printTime :: Sortable -> IO ()
printTime l = do
  let wonkySt = WonkyState {wonkyChance = 10, stuckChance = 10, previousAnswer = 0, stdGen = mkStdGen 143}
  putStrLn (" Algorithm    | Time            | Score    | n = " ++ show (length (fromSortBit l)))
  putStrLn ""
  printResultBits "TensortB4" l tensortB4 wonkySt
  printResultBits "TensortBL" l tensortBL wonkySt
  printResultBits "RSortP" l robustsortP wonkySt
  printResultBits "RSortB" l robustsortB wonkySt
  printResultBits "RSortM" l robustsortM wonkySt
  printResultSortable "Mergesort" l mergesort wonkySt
  printResultSortable "Quicksort" l quicksort wonkySt
  printResultSortable "Bubblesort" l bubblesort wonkySt
  putStrLn "----------------------------------------------------------"
