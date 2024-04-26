module Quicksort (quicksort) where

quicksort :: (Ord array) => [array] -> [array]
quicksort [] = []
quicksort (element : elements) =
  let lowerPartition = quicksort [array | array <- elements, array <= element]
      upperPartition = quicksort [array | array <- elements, array > element]
   in lowerPartition ++ [element] ++ upperPartition
