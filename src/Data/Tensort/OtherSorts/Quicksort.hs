module Data.Tensort.OtherSorts.Quicksort (quicksort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanOrEqualBit, lessThanOrEqualRecord)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..), WonkyState, fromSortBit, fromSortRec)

quicksort :: Sortable -> WonkyState -> (Sortable, WonkyState)
quicksort (SortBit []) wonkySt = (SortBit [], wonkySt)
quicksort (SortBit (x : xs)) wonkySt = do
  let (lowerPartition, x', upperPartition, wonkySt') = partitionBits xs x wonkySt
  let (lowerPartitionSorted, wonkySt'') = quicksort (SortBit lowerPartition) wonkySt'
  let (upperPartitionSorted, wonkySt''') = quicksort (SortBit upperPartition) wonkySt''
  (SortBit (fromSortBit lowerPartitionSorted ++ [x'] ++ fromSortBit upperPartitionSorted), wonkySt''')
quicksort (SortRec []) wonkySt = (SortRec [], wonkySt)
quicksort (SortRec (x : xs)) wonkySt = do
  let (lowerPartition, x', upperPartition, wonkySt') = partitionRecords xs x wonkySt
  let (lowerPartitionSorted, wonkySt'') = quicksort (SortRec lowerPartition) wonkySt'
  let (upperPartitionSorted, wonkySt''') = quicksort (SortRec upperPartition) wonkySt''
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
