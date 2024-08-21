module Benchmarking.PrintLargeTimeAndErrorRateComparison
  ( printLargeTimeAndErrorRateComparison,
  )
where

import Benchmarking.PadOut (padOut)
import Benchmarking.Score (getSingleRunErrorsScore, getTotalErrorsScore)
import Benchmarking.SortAlgsCompared (sortAlgsCompared)
import Data.Tensort.Utils.Types
  ( SortAlg,
  )
import Data.Time.Clock

printLargeTimeAndErrorRateComparison ::
  [Int] ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO ()
printLargeTimeAndErrorRateComparison [] _ _ _ _ = return ()
printLargeTimeAndErrorRateComparison
  (lengthExponent : remainingLengthExponents)
  i
  bubblesortCutoff
  wChance
  sChance = do
    printTime lengthExponent i bubblesortCutoff wChance sChance
    printLargeTimeAndErrorRateComparison
      remainingLengthExponents
      i
      bubblesortCutoff
      wChance
      sChance

printTime :: Int -> Int -> Int -> Int -> Int -> IO ()
printTime lengthExponent i bubblesortCutoff wChance sChance = do
  let listLength = 2 ^ lengthExponent
  putStrLn
    ( " Algorithm    | Time            | Score    | n = "
        ++ show listLength
    )
  putStrLn ""
  printResults listLength i bubblesortCutoff wChance sChance
  putStrLn "----------------------------------------------------------"

printResults :: Int -> Int -> Int -> Int -> Int -> IO ()
printResults listLength i bubblesortCutoff wChance sChance =
  foldr
    acc
    (return ())
    sortAlgsCompared
  where
    acc (sortAlg, sortName) io = do
      _ <- io
      printResultForAlg listLength i bubblesortCutoff sortAlg sortName wChance sChance

printResultForAlg :: Int -> Int -> Int -> SortAlg -> String -> Int -> Int -> IO ()
printResultForAlg listLength i bubblesortCutoff sortAlg sortName wChance sChance =
  if (listLength > bubblesortCutoff) && (sortName == "Bubblesort")
    then return ()
    else do
      startTime <- getCurrentTime
      let timeResult =
            getSingleRunErrorsScore
              sortAlg
              listLength
              wChance
              sChance
              143
      endTime <- getCurrentTimeArg timeResult
      let score = getTotalErrorsScore i sortAlg listLength wChance sChance `div` i
      composeResultString sortName startTime endTime score

-- This is to force times to record before and after the sort, otherwise
-- the functions run asynchronously
getCurrentTimeArg :: Int -> IO UTCTime
getCurrentTimeArg i = do
  let time = i * 0
  putStr (replicate time ' ')
  getCurrentTime

composeResultString :: String -> UTCTime -> UTCTime -> Int -> IO ()
composeResultString sortName startTime endTime score = do
  let nameString = padOut sortName 12
  let timeString = padOut (show (diffUTCTime endTime startTime)) 15
  let scoreString =
        padOut (show score) 8
  putStrLn
    ( " "
        ++ nameString
        ++ " | "
        ++ timeString
        ++ " | "
        ++ scoreString
        ++ " | "
    )
