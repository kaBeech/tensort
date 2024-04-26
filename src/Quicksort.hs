module Quicksort (quicksort) where

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (element : elements) =
  let lowerPartition = quicksort [array | array <- elements, array <= element]
      upperPartition = quicksort [array | array <- elements, array > element]
   in lowerPartition ++ [element] ++ upperPartition
