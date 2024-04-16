module Main where

import RobustsortHybrid qualified (rsorthbm, rsorthbq, rsorthmq)
import RobustsortParallel qualified (rsortpb, rsortpm, rsortpq)
import RobustsortSeries qualified (rsortsb, rsortsm, rsortsq)
import Vanillasort qualified (bsort, msort, qsort)

unsortedArray :: [String]
unsortedArray = ["4", "7", "2", "8", "9", "6", "3", "5", "1", "0"]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Vanillasort.bsort
  Vanillasort.msort
  putStrLn (unwords (Vanillasort.qsort unsortedArray))
  RobustsortSeries.rsortsb
  RobustsortSeries.rsortsm
  putStrLn (unwords (RobustsortSeries.rsortsq unsortedArray))
  RobustsortParallel.rsortpb
  RobustsortParallel.rsortpm
  RobustsortParallel.rsortpq
  RobustsortHybrid.rsorthbm
  RobustsortHybrid.rsorthbq
  RobustsortHybrid.rsorthmq
