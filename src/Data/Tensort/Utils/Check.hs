-- | This module provides the isSorted function, which checks if a list of
--   elements is sorted in ascending order.
module Data.Tensort.Utils.Check (isSorted) where

-- | Takes a Sortable list and returns True if the list is sorted in ascending
--   order and False otherwise.

-- | ==== __Examples__
-- >>> isSorted (SortBit [0, 1, 2, 3, 4])
-- True
--
-- >>> isSorted (SortBit [0, 1, 2, 4, 3])
-- False
isSorted :: [Ordering] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : remainingElements) =
  x <= y && isSorted (y : remainingElements)
