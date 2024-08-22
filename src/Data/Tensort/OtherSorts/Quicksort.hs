-- | This module provides the quicksort function for sorting lists using the
--   Sortable type
module Data.Tensort.OtherSorts.Quicksort (quicksort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..))

-- | Takes a Sortable and returns a sorted Sortable using a Quicksort algorithm

-- | ==== __Examples__
--  >>> quicksort (SortBit [16, 23, 4, 8, 15, 42])
--  SortBit [4,8,15,16,23,42]
--
--  >>> quicksort (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
--  SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
quicksort :: Sortable -> Sortable
quicksort (SortBit []) = SortBit []
quicksort (SortBit [x]) = SortBit [x]
quicksort (SortBit xs) = SortBit (quicksortBits xs)
quicksort (SortRec []) = SortRec []
quicksort (SortRec [x]) = SortRec [x]
quicksort (SortRec xs) = SortRec (quicksortRecs xs)

quicksortBits :: [Bit] -> [Bit]
quicksortBits [] = []
quicksortBits [x] = [x]
quicksortBits xs =
  let (lower, pivot, upper) = getPartitionsBits xs
   in quicksortBits lower ++ [pivot] ++ quicksortBits upper

getPartitionsBits :: [Bit] -> ([Bit], Bit, [Bit])
getPartitionsBits [] = error "From getPartitionsBits: empty input list"
getPartitionsBits [x] = ([], x, [])
getPartitionsBits (x : xs) = foldr acc ([], x, []) xs
  where
    acc :: Bit -> ([Bit], Bit, [Bit]) -> ([Bit], Bit, [Bit])
    acc y (lower, pivot, upper)
      | greaterThanBit y pivot = (lower, pivot, y : upper)
      | otherwise = (y : lower, pivot, upper)

quicksortRecs :: [Record] -> [Record]
quicksortRecs [] = []
quicksortRecs [x] = [x]
quicksortRecs xs =
  let (lower, pivot, upper) = getPartitionsRecs xs
   in quicksortRecs lower ++ [pivot] ++ quicksortRecs upper

getPartitionsRecs :: [Record] -> ([Record], Record, [Record])
getPartitionsRecs [] = error "From getPartitionsRecs: empty input list"
getPartitionsRecs [x] = ([], x, [])
getPartitionsRecs (x : xs) = foldr acc ([], x, []) xs
  where
    acc :: Record -> ([Record], Record, [Record]) -> ([Record], Record, [Record])
    acc y (lower, pivot, upper)
      | greaterThanRecord y pivot = (lower, pivot, y : upper)
      | otherwise = (y : lower, pivot, upper)
