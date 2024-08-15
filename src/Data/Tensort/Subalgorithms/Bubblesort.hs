module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions
  ( greaterThanBit,
    greaterThanRecord,
  )
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

bubblesort :: WonkyState -> Sortable -> (Sortable, WonkyState)
bubblesort wonkySt (SortBit bits) =
  let (result, wonkySt') =
        bublesortIterable greaterThanBit wonkySt bits 0 (length bits)
   in (SortBit result, wonkySt')
bubblesort wonkySt (SortRec recs) =
  let (result, wonkySt') =
        bublesortIterable greaterThanRecord wonkySt recs 0 (length recs)
   in (SortRec result, wonkySt')

bublesortIterable ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Int ->
  ([a], WonkyState)
bublesortIterable greaterThan wonkySt xs currentIndex i
  | length xs < 2 =
      (xs, wonkySt)
  | i < 1 =
      (xs, wonkySt)
  | currentIndex > length xs - 2 =
      bublesortIterable greaterThan wonkySt xs 0 (i - 1)
  | otherwise =
      let left = take currentIndex xs
          right = drop (currentIndex + 2) xs
          x = xs !! currentIndex
          y = xs !! (currentIndex + 1)
          (leftElemGreater, wonkySt') = greaterThan x y wonkySt
          swappedXs = left ++ [y] ++ [x] ++ right
       in if leftElemGreater
            then bublesortIterable greaterThan wonkySt' swappedXs (currentIndex + 1) i
            else bublesortIterable greaterThan wonkySt' xs (currentIndex + 1) i
