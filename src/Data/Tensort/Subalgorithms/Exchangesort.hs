module Data.Tensort.Subalgorithms.Exchangesort (exchangesort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..))

exchangesort :: Sortable -> Sortable
exchangesort (SortBit bits) = SortBit (exchangesortIterable greaterThanBit bits (length bits - 1) (length bits - 2))
exchangesort (SortRec recs) = SortRec (exchangesortIterable greaterThanRecord recs (length recs - 1) (length recs - 2))

exchangesortIterable :: (a -> a -> Bool) -> [a] -> Int -> Int -> [a]
exchangesortIterable greaterThan xs i j = do
  if i < 0
    then xs
    else
      if j < 0
        then exchangesortIterable greaterThan xs (i - 1) (length xs - 1)
        else
          if ((i > j) && greaterThan (xs !! j) (xs !! i)) || ((j > i) && greaterThan (xs !! i) (xs !! j))
            then exchangesortIterable greaterThan (swap xs i j) i (j - 1)
            else exchangesortIterable greaterThan xs i (j - 1)

swap :: [a] -> Int -> Int -> [a]
swap xs i j = do
  let x = xs !! i
  let y = xs !! j
  let mini = min i j
  let maxi = max i j
  let left = take mini xs
  let middle = take (maxi - mini - 1) (drop (mini + 1) xs)
  let right = drop (maxi + 1) xs
  left ++ [y] ++ middle ++ [x] ++ right
