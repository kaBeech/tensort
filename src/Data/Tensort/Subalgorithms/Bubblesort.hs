-- | This module provides the bubblesort function for sorting Sortable lists
module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions
  ( greaterThanBit,
    greaterThanRecord,
  )
import Data.Tensort.Utils.Types (Sortable (..))

-- | Takes a Sortable and returns a sorted Sortable using a Bubblesort
--   algorithm.

-- | ==== __Examples__
-- >>> bubblesort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> bubblesort (SortRec [(1, 16), (5, 23), (2, 4), (3, 8), (0, 15), (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
bubblesort :: Sortable -> Sortable
bubblesort (SortBit bits) =
  SortBit
    ( bublesortIterable greaterThanBit bits 0 (length bits)
    )
bubblesort (SortRec recs) =
  SortRec
    ( bublesortIterable greaterThanRecord recs 0 (length recs)
    )

bublesortIterable :: (Ord a) => (a -> a -> Bool) -> [a] -> Int -> Int -> [a]
bublesortIterable greaterThan xs currentIndex i
  | length xs < 2 = xs
  | i < 1 =
      xs
  | currentIndex > length xs - 2 =
      bublesortIterable greaterThan xs 0 (i - 1)
  | otherwise =
      let left = take currentIndex xs
          right = drop (currentIndex + 2) xs
          x = xs !! currentIndex
          y = xs !! (currentIndex + 1)
          leftElemGreater = greaterThan x y
          swappedXs = left ++ [y] ++ [x] ++ right
       in if leftElemGreater
            then bublesortIterable greaterThan swappedXs (currentIndex + 1) i
            else bublesortIterable greaterThan xs (currentIndex + 1) i
