module Data.Robustsort.Utils.Check (isSorted) where

import Data.Robustsort.Utils.ComparisonFunctions (lessThanInt, lessThanRecord)
import Data.Robustsort.Utils.Types (Sortable (..))

isSorted :: Sortable -> Bool
isSorted (SortInt []) = True
isSorted (SortInt [_]) = True
isSorted (SortInt (x : y : remainingElements)) = lessThanInt x y && isSorted (SortInt (y : remainingElements))
isSorted (SortRec []) = True
isSorted (SortRec [_]) = True
isSorted (SortRec (x : y : remainingElements)) = lessThanRecord x y && isSorted (SortRec (y : remainingElements))
