module RobustsortSeries (rsortsb, rsortsm, rsortsq) where

import Quicksort qualified (qsort)

rsortsb :: (Ord array) => [array] -> [array]
rsortsb [] = []
rsortsb elements = robustsortSeries Quicksort.qsort elements

rsortsm :: (Ord array) => [array] -> [array]
rsortsm [] = []
rsortsm elements = robustsortSeries Quicksort.qsort elements

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
