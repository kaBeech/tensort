module Data.Tensort.Subalgorithms.Exchangesort (exchangesort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

exchangesort :: WonkyState -> Sortable -> (Sortable, WonkyState)
exchangesort wonkySt (SortBit bits) = do
  let (result, wonkySt') = exchangesortIterable greaterThanBit wonkySt bits 0 (length bits - 1)
  (SortBit result, wonkySt')
exchangesort wonkySt (SortRec recs) = do
  let (result, wonkySt') = exchangesortIterable greaterThanRecord wonkySt recs 0 (length recs - 1)
  (SortRec result, wonkySt')

exchangesortIterable :: (Ord a) => (a -> a -> WonkyState -> (Bool, WonkyState)) -> WonkyState -> [a] -> Int -> Int -> ([a], WonkyState)
exchangesortIterable greaterThan wonkySt xs i j
  | i > length xs - 1 =
      (xs, wonkySt)
  | j < 0 =
      exchangesortIterable greaterThan wonkySt xs (i + 1) (length xs - 1)
  | i == j =
      exchangesortIterable greaterThan wonkySt xs i (j - 1)
  | otherwise =
      let mini = min i j
          maxi = max i j
          left = take mini xs
          middle = take (maxi - mini - 1) (drop (mini + 1) xs)
          right = drop (maxi + 1) xs
          x = xs !! mini
          y = xs !! maxi
          (leftElemGreater, wonkySt') = greaterThan x y wonkySt
          swappedXs = left ++ [y] ++ middle ++ [x] ++ right
       in if leftElemGreater
            then exchangesortIterable greaterThan wonkySt' swappedXs i (j - 1)
            else exchangesortIterable greaterThan wonkySt' xs i (j - 1)
