module Data.Tensort.OtherSorts.Quicksort (quicksort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord, lessThanOrEqualBit, lessThanOrEqualRecord)
import Data.Tensort.Utils.Types (Sortable (..), fromSortBit, fromSortRec)

quicksort :: Sortable -> Sortable
quicksort (SortBit []) = SortBit []
quicksort (SortBit (x : xs)) =
  let lowerPartition = quicksort (SortBit [a | a <- xs, lessThanOrEqualBit a x])
      upperPartition = quicksort (SortBit [a | a <- xs, greaterThanBit a x])
   in SortBit (fromSortBit lowerPartition ++ [x] ++ fromSortBit upperPartition)
quicksort (SortRec []) = SortRec []
quicksort (SortRec (x : xs)) =
  let lowerPartition = quicksort (SortRec [a | a <- xs, lessThanOrEqualRecord a x])
      upperPartition = quicksort (SortRec [a | a <- xs, greaterThanRecord a x])
   in SortRec (fromSortRec lowerPartition ++ [x] ++ fromSortRec upperPartition)
