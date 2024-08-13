module Main where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRB,
    robustsortRM,
    robustsortRP,
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
import Data.Tensort.Utils.Types (SortAlg, Sortable (..), WonkyState (..), fromSortBit)
import Data.Time.Clock
import System.Random (mkStdGen)

genUnsortedBits :: Int -> Sortable
genUnsortedBits n = randomizeList 143 (SortBit [1 .. n])

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
  let result = fst (sortAlg wonkySt l)
  endTime <- getCurrentTimeArg (length (fromSortBit result))
  composeResultString sortName startTime endTime result

printResultBits :: String -> Sortable -> SortAlg -> WonkyState -> IO ()
printResultBits sortName l sortAlg wonkySt = do
  startTime <- getCurrentTime
  let result = fst (sortAlg wonkySt l)
  endTime <- getCurrentTimeArg (head (fromSortBit result))
  composeResultString sortName startTime endTime result

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
  -- printErrorRateComparison 1000
  -- printErrorSpread 100000
  -- Eventually I hope to turn that 14 into a 20
  printTimes (map (\x -> (genUnsortedBits (genTestPeriod x), 143)) [3 .. 14])

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
  printResultBits "RSortRM" l robustsortRM wonkySt
  printResultBits "RSortRB" l robustsortRB wonkySt
  printResultBits "RSortRP" l robustsortRP wonkySt
  printResultBits "RSortM" l robustsortM wonkySt
  printResultBits "RSortB" l robustsortB wonkySt
  printResultBits "RSortP" l robustsortP wonkySt
  putStrLn "----------------------------------------------------------"

sortAlgsCompared :: [(WonkyState -> Sortable -> (Sortable, WonkyState), String)]
sortAlgsCompared =
  [ (bubblesort, "Bubblesort"),
    (mergesort, "Mergesort"),
    (quicksort, "Quicksort"),
    (bogosort, "Bogosort"),
    (permutationsort, "Permutationsort"),
    (magicsort, "Magicsort"),
    (supersortP, "SupersortP"),
    (supersortB, "SupersortB"),
    (supersortM, "SupersortM"),
    (robustsortP, "RobustsortP"),
    (robustsortB, "RobustsortB"),
    (robustsortRM, "RobustsortRM"),
    (exchangesort, "Exchangesort")
  ]

printErrorSpread :: Int -> IO ()
printErrorSpread i = foldr acc (return ()) sortAlgsCompared
  where
    acc sortAlg io = do
      _ <- io
      printErrorSpreadForAlg i sortAlg

printErrorSpreadForAlg :: Int -> (WonkyState -> Sortable -> (Sortable, WonkyState), String) -> IO ()
printErrorSpreadForAlg i (sortAlg, sortName) = do
  putStrLn (sortName ++ " Error Spread")
  putStrLn "-------------------------"
  let (oneTwoThree, oneThreeTwo, twoOneThree, twoThreeOne, threeOneTwo, threeTwoOne) = getErrorSpread (0, 0, 0, 0, 0, 0) [1 .. i] sortAlg
  putStrLn ("123: " ++ show oneTwoThree)
  putStrLn ("132: " ++ show oneThreeTwo)
  putStrLn ("213: " ++ show twoOneThree)
  putStrLn ("231: " ++ show twoThreeOne)
  putStrLn ("312: " ++ show threeOneTwo)
  putStrLn ("321: " ++ show threeTwoOne)
  putStrLn ""

getErrorSpread :: (Int, Int, Int, Int, Int, Int) -> [Int] -> (WonkyState -> Sortable -> (Sortable, WonkyState)) -> (Int, Int, Int, Int, Int, Int)
getErrorSpread errorSpread [] _ = errorSpread
getErrorSpread errorSpread (x : xs) sortAlg = do
  let (oneTwoThree, oneThreeTwo, twoOneThree, twoThreeOne, threeOneTwo, threeTwoOne) = getErrorSpread errorSpread xs sortAlg
  let l = randomizeList x (SortBit [1, 2, 3])
  let wonkySt = WonkyState {wonkyChance = 10, stuckChance = 0, previousAnswer = 0, stdGen = mkStdGen x}
  let result = fst (sortAlg wonkySt l)
  if result == SortBit [1, 2, 3]
    then (oneTwoThree + 1, oneThreeTwo, twoOneThree, twoThreeOne, threeOneTwo, threeTwoOne)
    else
      if result == SortBit [1, 3, 2]
        then (oneTwoThree, oneThreeTwo + 1, twoOneThree, twoThreeOne, threeOneTwo, threeTwoOne)
        else
          if result == SortBit [2, 1, 3]
            then (oneTwoThree, oneThreeTwo, twoOneThree + 1, twoThreeOne, threeOneTwo, threeTwoOne)
            else
              if result == SortBit [2, 3, 1]
                then (oneTwoThree, oneThreeTwo, twoOneThree, twoThreeOne + 1, threeOneTwo, threeTwoOne)
                else
                  if result == SortBit [3, 1, 2]
                    then (oneTwoThree, oneThreeTwo, twoOneThree, twoThreeOne, threeOneTwo + 1, threeTwoOne)
                    else
                      if result == SortBit [3, 2, 1]
                        then (oneTwoThree, oneThreeTwo, twoOneThree, twoThreeOne, threeOneTwo, threeTwoOne + 1)
                        else error ("From getErrorSpread: Unrecognized result: " ++ show result)

printErrorRateComparison :: Int -> IO ()
printErrorRateComparison i = foldr acc (return ()) sortAlgsCompared
  where
    acc sortAlg io = do
      _ <- io
      printErrorRateComparisonForAlg i sortAlg

printErrorRateComparisonForAlg :: Int -> (WonkyState -> Sortable -> (Sortable, WonkyState), String) -> IO ()
printErrorRateComparisonForAlg i (sortAlg, sortName) = putStrLn (padOut (sortName ++ " Errors: ") 24 ++ show (getTotalErrorsScore i sortAlg))

getTotalErrorsScore :: Int -> (WonkyState -> Sortable -> (Sortable, WonkyState)) -> Int
getTotalErrorsScore i sortAlg = foldr acc 0 [1 .. i]
  where
    acc x totalScore = do
      let l = randomizeList x (SortBit [1 .. 3])
      let wonkySt = WonkyState {wonkyChance = 10, stuckChance = 0, previousAnswer = 0, stdGen = mkStdGen x}
      let result = fst (sortAlg wonkySt l)
      let roundScore = getTotalPositionalErrors (fromSortBit result)
      totalScore + roundScore
