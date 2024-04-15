module Main where

import RobustsortHybrid qualified (rsorthbm, rsorthbq, rsorthmq)
import RobustsortParallel qualified (rsortpb, rsortpm, rsortpq)
import RobustsortSeries qualified (rsortsb, rsortsm, rsortsq)
import Vanillasort qualified (bsort, msort, qsort)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Vanillasort.bsort
  Vanillasort.msort
  Vanillasort.qsort
  RobustsortSeries.rsortsb
  RobustsortSeries.rsortsm
  RobustsortSeries.rsortsq
  RobustsortParallel.rsortpb
  RobustsortParallel.rsortpm
  RobustsortParallel.rsortpq
  RobustsortHybrid.rsorthbm
  RobustsortHybrid.rsorthbq
  RobustsortHybrid.rsorthmq
