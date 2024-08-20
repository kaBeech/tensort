module Benchmarking.PrintTimes (printTimes) where

import Benchmarking.PadOut (padOut)
import Benchmarking.Score (getSingleRunErrorsScore, getTotalErrorsScore)
import Benchmarking.SortAlgsCompared (sortAlgsCompared)
import Data.Tensort.Utils.Types
  ( SortAlg,
  )
import Data.Time.Clock

printTimes :: [Int] -> Int -> Int -> Int -> IO ()
printTimes [] _ _ _ = return ()
printTimes (lengthExponent : remainingLengthExponents) i wChance sChance = do
  printTime lengthExponent i wChance sChance
  printTimes remainingLengthExponents i wChance sChance

printTime :: Int -> Int -> Int -> Int -> IO ()
printTime lengthExponent i wChance sChance = do
  let listLength = 2 ^ lengthExponent
  putStrLn
    ( " Algorithm    | Time            | Score    | n = "
        ++ show listLength
    )
  putStrLn ""
  printResults listLength i wChance sChance
  putStrLn "----------------------------------------------------------"

printResults :: Int -> Int -> Int -> Int -> IO ()
printResults listLength i wChance sChance = foldr acc (return ()) sortAlgsCompared
  where
    acc (sortAlg, sortName) io = do
      _ <- io
      printResultForAlg i sortAlg sortName listLength wChance sChance

printResultForAlg :: Int -> SortAlg -> String -> Int -> Int -> Int -> IO ()
printResultForAlg i sortAlg sortName listLength wChance sChance = do
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
