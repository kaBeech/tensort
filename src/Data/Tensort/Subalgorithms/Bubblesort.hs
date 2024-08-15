module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions
  ( greaterThanBit,
    greaterThanRecord,
  )
import Data.Tensort.Utils.Types (Sortable (..))

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
