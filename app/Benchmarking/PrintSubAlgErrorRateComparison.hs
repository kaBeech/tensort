module Benchmarking.PrintSubAlgErrorRateComparison (printSubAlgErrorRateComparison) where

import Benchmarking.PadOut (padOut)
import Benchmarking.Score (getTotalErrorsScore)
import Benchmarking.SubAlgsCompared (subAlgsCompared)
import Data.Tensort.Utils.Types
  ( Sortable (..),
    WonkyState (..),
  )

printSubAlgErrorRateComparison :: Int -> Int -> Int -> IO ()
printSubAlgErrorRateComparison i wChance sChance = foldr acc (return ()) subAlgsCompared
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
    ( padOut (sortName ++ " Errors: ") 24
        ++ show (getTotalErrorsScore i sortAlg 3 wChance sChance)
    )
