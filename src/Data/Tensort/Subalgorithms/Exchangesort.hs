-- | This module provides the bubblesort function for sorting lists using the
--   Sortable type.
module Data.Tensort.Subalgorithms.Exchangesort (exchangesort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..))

-- | Takes a Sortable and returns a sorted Sortable using an Exchangesort
--   algorithm.

-- | ==== __Examples__
-- >>> exchangesort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> exchangesort (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
exchangesort :: Sortable -> Sortable
exchangesort (SortBit bits) = SortBit (exchangesortIterable greaterThanBit bits 0 (length bits - 1))
exchangesort (SortRec recs) = SortRec (exchangesortIterable greaterThanRecord recs 0 (length recs - 1))

exchangesortIterable :: (Ord a) => (a -> a -> Bool) -> [a] -> Int -> Int -> [a]
exchangesortIterable greaterThan xs i j
  | i > length xs - 1 =
      xs
  | j < 0 =
      exchangesortIterable greaterThan xs (i + 1) (length xs - 1)
  | i == j =
      exchangesortIterable greaterThan xs i (j - 1)
  | otherwise =
      let mini = min i j
          maxi = max i j
          left = take mini xs
          middle = take (maxi - mini - 1) (drop (mini + 1) xs)
          right = drop (maxi + 1) xs
          x = xs !! mini
          y = xs !! maxi
          leftElemGreater = greaterThan x y
          swappedXs = left ++ [y] ++ middle ++ [x] ++ right
       in if leftElemGreater
            then exchangesortIterable greaterThan swappedXs i (j - 1)
            else exchangesortIterable greaterThan xs i (j - 1)
