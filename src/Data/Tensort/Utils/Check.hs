-- | This module provides the isSorted function, which checks if a list of
--   elements is sorted in ascending order.
module Data.Tensort.Utils.Check (isSorted) where

import Data.Tensort.Utils.ComparisonFunctions
  ( lessThanOrEqualBit,
    lessThanOrEqualRecord,
  )
import Data.Tensort.Utils.Types (Sortable (..))

-- | Takes a Sortable list and returns True if the list is sorted in ascending
--   order and False otherwise.

-- | ==== __Examples__
-- >>> isSorted (SortBit [0, 1, 2, 3, 4])
-- True
--
-- >>> isSorted (SortBit [0, 1, 2, 4, 3])
-- False
isSorted :: Sortable -> Bool
isSorted (SortBit []) = True
isSorted (SortBit [_]) = True
isSorted (SortBit (x : y : remainingElements)) =
  lessThanOrEqualBit x y && isSorted (SortBit (y : remainingElements))
isSorted (SortRec []) = True
isSorted (SortRec [_]) = True
isSorted (SortRec (x : y : remainingElements)) =
  lessThanOrEqualRecord x y && isSorted (SortRec (y : remainingElements))
