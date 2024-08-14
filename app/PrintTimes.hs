module PrintTimes (printTimes) where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRB,
    robustsortRM,
    robustsortRP,
  )
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortBL)
import Data.Tensort.Utils.Score (getTotalPositionalErrors)
import Data.Tensort.Utils.Types
  ( SortAlg,
    Sortable (..),
    WonkyState (..),
    fromSortBit,
  )
import Data.Time.Clock
import PadOut (padOut)
import System.Random (mkStdGen)

printTimes :: [(Sortable, Int)] -> IO ()
printTimes [] = return ()
printTimes (x : xs) = do
  printTime x
  printTimes xs

printTime :: (Sortable, Int) -> IO ()
printTime (l, seed) = do
  let wonkySt =
        WonkyState
          { wonkyChance = 10,
            stuckChance = 0,
            previousAnswer = 0,
            stdGen = mkStdGen seed
          }
  putStrLn
    ( " Algorithm    | Time            | Score    | n = "
        ++ show (length (fromSortBit l))
    )
  putStrLn ""
  printResult "TensortBL" l tensortBL wonkySt
  printResult "Mergesort" l mergesort wonkySt
  printResult "Quicksort" l quicksort wonkySt
  printResult "Bubblesort" l bubblesort wonkySt
  printResult "RSortRM" l robustsortRM wonkySt
  printResult "RSortRB" l robustsortRB wonkySt
  printResult "RSortRP" l robustsortRP wonkySt
  printResult "RSortM" l robustsortM wonkySt
  printResult "RSortB" l robustsortB wonkySt
  printResult "RSortP" l robustsortP wonkySt
  putStrLn "----------------------------------------------------------"

printResult :: String -> Sortable -> SortAlg -> WonkyState -> IO ()
printResult sortName l sortAlg wonkySt = do
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
