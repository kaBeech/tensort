module Benchmarking.PrintDeckShuffleErrors (printDeckShuffleErrors) where

import Benchmarking.PadOut (padOut)
import Benchmarking.Score (getTotalErrorsScore)
import Benchmarking.SortAlgsCompared (sortAlgsCompared)
import Data.Tensort.Utils.Types
  ( Sortable (..),
    WonkyState (..),
  )

printDeckShuffleErrors :: Int -> Int -> Int -> IO ()
printDeckShuffleErrors i wChance sChance = foldr acc (return ()) sortAlgsCompared
  where
    acc sortAlg io = do
      _ <- io
      printErrorRateComparisonForAlg i sortAlg wChance sChance

printErrorRateComparisonForAlg ::
  Int ->
  (WonkyState -> Sortable -> (Sortable, WonkyState), String) ->
  Int ->
  Int ->
  IO ()
printErrorRateComparisonForAlg i (sortAlg, sortName) wChance sChance =
  putStrLn
    ( padOut (sortName ++ " Errors: ") 30
        ++ show (getTotalErrorsScore i sortAlg 56 wChance sChance `div` i)
    )
