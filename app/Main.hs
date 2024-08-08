module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    supersortB,
    supersortM,
    supersortP,
  )
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
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
        -- ++ show (fromSortBit result)
    )

main :: IO ()
main = do
  -- Eventually I hope to turn that 14 into a 20
  -- printTimes (map (\x -> (genUnsortedBits (genTestPeriod x), 143)) [3 .. 20])
  printErrorRateComparison 1000

printTimes :: [(Sortable, Int)] -> IO ()
printTimes [] = return ()
printTimes (x : xs) = do
  printTime x
  printTimes xs

printTime :: (Sortable, Int) -> IO ()
printTime (l, seed) = do
  let wonkySt = WonkyState {wonkyChance = 10, stuckChance = 0, previousAnswer = 0, stdGen = mkStdGen seed}
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

sortAlgsCompared :: [(Sortable -> WonkyState -> (Sortable, WonkyState), String)]
sortAlgsCompared =
  [ (bubblesort, "Bubblesort"),
    (exchangesort, "Exchangesort"),
    (mergesort, "Mergesort"),
    (quicksort, "Quicksort"),
    (bogosort, "Bogosort"),
    (permutationsort, "Permutationsort"),
    (magicsort, "Magicsort"),
    (supersortP, "SupersortP"),
    (supersortB, "SupersortB"),
    (supersortM, "SupersortM")
  ]

printErrorRateComparison :: Int -> IO ()
printErrorRateComparison i = foldr acc (return ()) sortAlgsCompared
  where
    acc sortAlg io = do
      _ <- io
      printErrorRateComparisonForAlg i sortAlg

printErrorRateComparisonForAlg :: Int -> (Sortable -> WonkyState -> (Sortable, WonkyState), String) -> IO ()
printErrorRateComparisonForAlg i (sortAlg, sortName) = do
  putStrLn (padOut (sortName ++ " Errors: ") 24 ++ show (getTotalErrorsScore i sortAlg))

getTotalErrorsScore :: Int -> (Sortable -> WonkyState -> (Sortable, WonkyState)) -> Int
getTotalErrorsScore i sortAlg = foldr acc 0 [1 .. i]
  where
    acc x totalScore = do
      let l = randomizeList (SortBit [1 .. 3]) x
      let wonkySt = WonkyState {wonkyChance = 10, stuckChance = 0, previousAnswer = 0, stdGen = mkStdGen x}
      let result = fst (sortAlg l wonkySt)
      let roundScore = getTotalPositionalErrors (fromSortBit result)
      totalScore + roundScore
