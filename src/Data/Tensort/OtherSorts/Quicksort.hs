module Data.Tensort.OtherSorts.Quicksort (quicksort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanInt, greaterThanRecord, lessThanOrEqualInt, lessThanOrEqualRecord)
import Data.Tensort.Utils.Types (Sortable (..), fromSortInt, fromSortRec)

quicksort :: Sortable -> Sortable
quicksort (SortInt []) = SortInt []
quicksort (SortInt (x : xs)) =
  let lowerPartition = quicksort (SortInt [a | a <- xs, lessThanOrEqualInt a x])
      upperPartition = quicksort (SortInt [a | a <- xs, greaterThanInt a x])
   in SortInt (fromSortInt lowerPartition ++ [x] ++ fromSortInt upperPartition)
quicksort (SortRec []) = SortRec []
quicksort (SortRec (x : xs)) =
  let lowerPartition = quicksort (SortRec [a | a <- xs, lessThanOrEqualRecord a x])
      upperPartition = quicksort (SortRec [a | a <- xs, greaterThanRecord a x])
   in SortRec (fromSortRec lowerPartition ++ [x] ++ fromSortRec upperPartition)
