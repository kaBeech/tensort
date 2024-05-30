module Data.Tensort.Utils.Check (isSorted) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Sortable (..))

isSorted :: Sortable -> Bool
isSorted (SortBit []) = True
isSorted (SortBit [_]) = True
isSorted (SortBit (x : y : remainingElements)) = lessThanBit x y && isSorted (SortBit (y : remainingElements))
isSorted (SortRec []) = True
isSorted (SortRec [_]) = True
isSorted (SortRec (x : y : remainingElements)) = lessThanRecord x y && isSorted (SortRec (y : remainingElements))
