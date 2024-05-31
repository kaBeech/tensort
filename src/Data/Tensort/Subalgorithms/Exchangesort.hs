module Data.Tensort.Subalgorithms.Exchangesort (exchangesort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..))

exchangesort :: Sortable -> Sortable
exchangesort (SortBit bits) = SortBit (exchangesortIterable bits (length bits - 1) (length bits - 2) greaterThanBit)
exchangesort (SortRec recs) = SortRec (exchangesortIterable recs (length recs - 1) (length recs - 2) greaterThanRecord)

exchangesortIterable :: [a] -> Int -> Int -> (a -> a -> Bool) -> [a]
exchangesortIterable xs i j greaterThan = do
  if i < 0
    then xs
    else
      if j < 0
        then exchangesortIterable xs (i - 1) (length xs - 1) greaterThan
        else
          if ((i > j) && greaterThan (xs !! j) (xs !! i)) || ((i < j) && greaterThan (xs !! i) (xs !! j))
            then exchangesortIterable (swap xs i j) i (j - 1) greaterThan
            else exchangesortIterable xs i (j - 1) greaterThan

swap :: [a] -> Int -> Int -> [a]
swap xs i j = do
  let x = xs !! i
  let y = xs !! j
  let left = take j xs
  let middle = take (i - j - 1) (drop (j + 1) xs)
  let right = drop (i + 1) xs
  left ++ [y] ++ middle ++ [x] ++ right
