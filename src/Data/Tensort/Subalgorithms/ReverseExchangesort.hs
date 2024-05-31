module Data.Tensort.Subalgorithms.ReverseExchangesort (reverseExchangesort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..))

reverseExchangesort :: Sortable -> Sortable
reverseExchangesort (SortBit bits) = SortBit (reverseExchangesortIterable bits (length bits - 1) (length bits - 2) greaterThanBit)
reverseExchangesort (SortRec recs) = SortRec (reverseExchangesortIterable recs (length recs - 1) (length recs - 2) greaterThanRecord)

reverseExchangesortIterable :: [a] -> Int -> Int -> (a -> a -> Bool) -> [a]
reverseExchangesortIterable xs i j greaterThan = do
  if i < 0
    then xs
    else
      if j < 0
        then reverseExchangesortIterable xs (i - 1) (length xs - 1) greaterThan
        else
          if ((i > j) && greaterThan (xs !! j) (xs !! i)) || ((i < j) && greaterThan (xs !! i) (xs !! j))
            then reverseExchangesortIterable (swap xs i j) i (j - 1) greaterThan
            else reverseExchangesortIterable xs i (j - 1) greaterThan

swap :: [a] -> Int -> Int -> [a]
swap xs i j = do
  let x = xs !! i
  let y = xs !! j
  let left = take j xs
  let middle = take (i - j - 1) (drop (j + 1) xs)
  let right = drop (i + 1) xs
  left ++ [y] ++ middle ++ [x] ++ right
