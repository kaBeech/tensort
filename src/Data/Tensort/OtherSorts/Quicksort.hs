module Data.Tensort.OtherSorts.Quicksort (quicksort) where

import Data.Tensort.Utils.ComparisonFunctions
  ( greaterThanBit,
    greaterThanRecord,
  )
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..), WonkyState)

quicksort :: WonkyState -> Sortable -> (Sortable, WonkyState)
quicksort wonkySt (SortBit []) = (SortBit [], wonkySt)
quicksort wonkySt (SortBit [x]) = (SortBit [x], wonkySt)
quicksort wonkySt (SortBit xs) =
  let (result, wonkySt') = quicksortBits wonkySt xs
   in (SortBit result, wonkySt')
quicksort wonkySt (SortRec []) = (SortRec [], wonkySt)
quicksort wonkySt (SortRec [x]) = (SortRec [x], wonkySt)
quicksort wonkySt (SortRec xs) =
  let (result, wonkySt') = quicksortRecs wonkySt xs
   in (SortRec result, wonkySt')

quicksortBits :: WonkyState -> [Bit] -> ([Bit], WonkyState)
quicksortBits wonkySt [] = ([], wonkySt)
quicksortBits wonkySt [x] = ([x], wonkySt)
quicksortBits wonkySt xs = do
  let ((lower, pivot, upper), wonkySt') = getPartitionsBits wonkySt xs
  let (lowerSorted, wonkySt'') = quicksortBits wonkySt' lower
  let (upperSorted, wonkySt''') = quicksortBits wonkySt'' upper
  (lowerSorted ++ [pivot] ++ upperSorted, wonkySt''')

getPartitionsBits :: WonkyState -> [Bit] -> (([Bit], Bit, [Bit]), WonkyState)
getPartitionsBits _ [] = error "From getPartitionsBits: empty input list"
getPartitionsBits wonkySt [x] = (([], x, []), wonkySt)
getPartitionsBits wonkySt (x : xs) = foldr acc (([], x, []), wonkySt) xs
  where
    acc ::
      Bit ->
      (([Bit], Bit, [Bit]), WonkyState) ->
      (([Bit], Bit, [Bit]), WonkyState)
    acc y ((lower, pivot, upper), wonkySt') =
      let (result, wonkySt'') = greaterThanBit y pivot wonkySt'
       in if result
            then ((lower, pivot, y : upper), wonkySt'')
            else ((y : lower, pivot, upper), wonkySt'')

quicksortRecs :: WonkyState -> [Record] -> ([Record], WonkyState)
quicksortRecs wonkySt [] = ([], wonkySt)
quicksortRecs wonkySt [x] = ([x], wonkySt)
quicksortRecs wonkySt xs = do
  let ((lower, pivot, upper), wonkySt') = getPartitionsRecs wonkySt xs
  let (lowerSorted, wonkySt'') = quicksortRecs wonkySt' lower
  let (upperSorted, wonkySt''') = quicksortRecs wonkySt'' upper
  (lowerSorted ++ [pivot] ++ upperSorted, wonkySt''')

getPartitionsRecs ::
  WonkyState ->
  [Record] ->
  (([Record], Record, [Record]), WonkyState)
getPartitionsRecs _ [] = error "From getPartitionsRecs: empty input list"
getPartitionsRecs wonkySt [x] = (([], x, []), wonkySt)
getPartitionsRecs wonkySt (x : xs) = foldr acc (([], x, []), wonkySt) xs
  where
    acc ::
      Record ->
      (([Record], Record, [Record]), WonkyState) ->
      (([Record], Record, [Record]), WonkyState)
    acc y ((lower, pivot, upper), wonkySt') =
      let (result, wonkySt'') = greaterThanRecord y pivot wonkySt'
       in if result
            then ((lower, pivot, y : upper), wonkySt'')
            else ((y : lower, pivot, upper), wonkySt'')
