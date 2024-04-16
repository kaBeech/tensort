module Quicksort (qsort) where

qsort :: (Ord array) => [array] -> [array]
qsort [] = []
qsort (element : elements) =
  let lowerPartition = qsort [array | array <- elements, array <= element]
      upperPartition = qsort [array | array <- elements, array > element]
   in lowerPartition ++ [element] ++ upperPartition
