-- | This module provides an implementation of the quicksort algorithm suitable
--   for sorting lists.
module Data.Tensort.OtherSorts.Quicksort (quicksort) where

-- | Takes a list and returns a sorted list using a Quicksort
--   algorithm.

-- | ==== __Examples__
--  >>> quicksort ([16, 23, 4, 8, 15, 42] :: [Int])
--  [4,8,15,16,23,42]
--
--  >>> quicksort ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
--  [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort xs = quicksort' xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' [x] = [x]
quicksort' xs =
  let (lower, pivot, upper) = getPartitionsBits xs
   in quicksort' lower ++ [pivot] ++ quicksort' upper

getPartitionsBits :: (Ord a) => [a] -> ([a], a, [a])
getPartitionsBits [] = error "From getPartitionsBits: empty input list"
getPartitionsBits [x] = ([], x, [])
getPartitionsBits (x : xs) = foldr acc ([], x, []) xs
  where
    acc :: (Ord a) => a -> ([a], a, [a]) -> ([a], a, [a])
    acc y (lower, pivot, upper)
      | y > pivot = (lower, pivot, y : upper)
      | otherwise = (y : lower, pivot, upper)
