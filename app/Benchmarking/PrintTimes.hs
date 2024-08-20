module Benchmarking.PrintTimes (printTimes) where

import Benchmarking.PadOut (padOut)
import Benchmarking.SortAlgsCompared (sortAlgsCompared)
import Data.Tensort.Utils.Score (getTotalPositionalErrors)
import Data.Tensort.Utils.Types
  ( SortAlg,
    Sortable (..),
    WonkyState (..),
    fromSortBit,
  )
import Data.Time.Clock
import System.Random (mkStdGen)

printTimes :: [(Sortable, Int)] -> Int -> Int -> IO ()
printTimes [] _ _ = return ()
printTimes (x : xs) wChance sChance = do
  printTime x wChance sChance
  printTimes xs wChance sChance

printTime :: (Sortable, Int) -> Int -> Int -> IO ()
printTime (l, seed) wChance sChance = do
  let wonkySt =
        WonkyState
          { wonkyChance = wChance,
            stuckChance = sChance,
            previousAnswer = 0,
            stdGen = mkStdGen seed
          }
  putStrLn
    ( " Algorithm    | Time            | Score    | n = "
        ++ show (length (fromSortBit l))
    )
  putStrLn ""
  printResults l wonkySt
  putStrLn "----------------------------------------------------------"

printResults :: Sortable -> WonkyState -> IO ()
printResults l wonkySt = foldr acc (return ()) sortAlgsCompared
  where
    acc (sortAlg, sortName) io = do
      _ <- io
      printResultForAlg sortName l sortAlg wonkySt

printResultForAlg :: String -> Sortable -> SortAlg -> WonkyState -> IO ()
printResultForAlg sortName l sortAlg wonkySt = do
  startTime <- getCurrentTime
  let result = fst (sortAlg wonkySt l)
  endTime <- getCurrentTimeArg (head (fromSortBit result))
  composeResultString sortName startTime endTime result

-- This is to force times to record before and after the sort, otherwise
-- the functions run asynchronously
getCurrentTimeArg :: Int -> IO UTCTime
getCurrentTimeArg i = do
  let time = i * 0
  putStr (replicate time ' ')
  getCurrentTime

composeResultString :: String -> UTCTime -> UTCTime -> Sortable -> IO ()
composeResultString sortName startTime endTime result = do
  let nameString = padOut sortName 12
  let timeString = padOut (show (diffUTCTime endTime startTime)) 15
  let scoreString =
        padOut (show (getTotalPositionalErrors (fromSortBit result))) 8
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
