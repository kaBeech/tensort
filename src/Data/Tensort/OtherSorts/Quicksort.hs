module Data.Tensort.OtherSorts.Quicksort (quicksort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanOrEqualBit, lessThanOrEqualRecord)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..), WonkyState, fromSortBit, fromSortRec)

quicksort :: WonkyState -> Sortable -> (Sortable, WonkyState)
quicksort wonkySt (SortBit []) = (SortBit [], wonkySt)
quicksort wonkySt (SortBit (x : xs)) = do
  let (lowerPartition, x', upperPartition, wonkySt') = partitionBits xs x wonkySt
  let (lowerPartitionSorted, wonkySt'') = quicksort wonkySt' (SortBit lowerPartition)
  let (upperPartitionSorted, wonkySt''') = quicksort wonkySt'' (SortBit upperPartition)
  (SortBit (fromSortBit lowerPartitionSorted ++ [x'] ++ fromSortBit upperPartitionSorted), wonkySt''')
quicksort wonkySt (SortRec []) = (SortRec [], wonkySt)
quicksort wonkySt (SortRec (x : xs)) = do
  let (lowerPartition, x', upperPartition, wonkySt') = partitionRecords xs x wonkySt
  let (lowerPartitionSorted, wonkySt'') = quicksort wonkySt' (SortRec lowerPartition)
  let (upperPartitionSorted, wonkySt''') = quicksort wonkySt'' (SortRec upperPartition)
  (SortRec (fromSortRec lowerPartitionSorted ++ [x'] ++ fromSortRec upperPartitionSorted), wonkySt''')

partitionBits :: [Bit] -> Bit -> WonkyState -> ([Bit], Bit, [Bit], WonkyState)
partitionBits [] pivot wonkySt = ([], pivot, [], wonkySt)
partitionBits (x : xs) pivot wonkySt = do
  let (result, wonkySt') = lessThanOrEqualBit x pivot wonkySt
  let (lowerPartition, pivot', upperPartition, wonkySt'') = partitionBits xs pivot wonkySt'
  if result
    then (x : lowerPartition, pivot', upperPartition, wonkySt'')
    else (lowerPartition, pivot', x : upperPartition, wonkySt'')

partitionRecords :: [Record] -> Record -> WonkyState -> ([Record], Record, [Record], WonkyState)
partitionRecords [] pivot wonkySt = ([], pivot, [], wonkySt)
partitionRecords (x : xs) pivot wonkySt = do
  let (result, wonkySt') = lessThanOrEqualRecord x pivot wonkySt
  let (lowerPartition, pivot', upperPartition, wonkySt'') = partitionRecords xs pivot wonkySt'
  if result
    then (x : lowerPartition, pivot', upperPartition, wonkySt'')
    else (lowerPartition, pivot', x : upperPartition, wonkySt'')
