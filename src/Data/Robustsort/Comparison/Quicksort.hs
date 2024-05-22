module Data.Robustsort.Comparison.Quicksort (quicksort) where

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) =
  let lowerPartition = quicksort [a | a <- xs, a <= x]
      upperPartition = quicksort [a | a <- xs, a > x]
   in lowerPartition ++ [x] ++ upperPartition
