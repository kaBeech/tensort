module RobustsortSeries (rsortsb, rsortsm, rsortsq) where

import Quicksort qualified (qsort)

rsortsb :: IO ()
rsortsb = putStrLn "rsortsq successfully called!"

rsortsm :: IO ()
rsortsm = putStrLn "rsortsm successfully called!"

rsortsq :: (Ord array) => [array] -> [array]
rsortsq [] = []
rsortsq elements = robustsortSeries Quicksort.qsort elements

robustsortSeries :: (Ord array) => ([array] -> [array]) -> [array] -> [array]
robustsortSeries sortFunction array =
  let firstResult = sortFunction array
      secondResult = sortFunction array
   in if firstResult == secondResult
        then firstResult
        else robustsortSeries sortFunction array
