module RobustsortHybrid (rsorthbm, rsorthbq, rsorthmq) where

import Bubblesort qualified (bsort)
import Mergesort qualified (msort)
import Quicksort qualified (qsort)

rsorthbm :: (Ord array) => [array] -> [array]
rsorthbm = robustsortHybrid Mergesort.msort Bubblesort.bsort

rsorthbq :: (Ord array) => [array] -> [array]
rsorthbq = robustsortHybrid Quicksort.qsort Bubblesort.bsort

rsorthmq :: (Ord array) => [array] -> [array]
rsorthmq = robustsortHybrid Mergesort.msort Quicksort.qsort

robustsortHybrid :: (Ord array) => ([array] -> [array]) -> ([array] -> [array]) -> [array] -> [array]
robustsortHybrid sortFunction1 sortFunction2 array =
  let firstResult = sortFunction1 array
      secondResult = sortFunction2 array
   in if firstResult == secondResult
        then firstResult
        else robustsortHybrid sortFunction1 sortFunction2 array
