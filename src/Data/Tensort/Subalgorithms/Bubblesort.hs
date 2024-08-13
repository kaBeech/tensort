module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..), WonkyState)

bubblesort :: WonkyState -> Sortable -> (Sortable, WonkyState)
bubblesort wonkySt (SortBit bits) = do
  let (result, wonkySt') = foldr acc ([], wonkySt) bits
  (SortBit result, wonkySt')
  where
    acc :: Bit -> ([Bit], WonkyState) -> ([Bit], WonkyState)
    acc x (xs, wonkySt') = bubblesortSinglePass lessThanBit wonkySt' x xs
bubblesort wonkySt (SortRec recs) = do
  let (result, wonkySt') = foldr acc ([], wonkySt) recs
  (SortRec result, wonkySt')
  where
    acc :: Record -> ([Record], WonkyState) -> ([Record], WonkyState)
    acc x (xs, wonkySt') = bubblesortSinglePass lessThanRecord wonkySt' x xs

bubblesortSinglePass :: (a -> a -> WonkyState -> (Bool, WonkyState)) -> WonkyState -> a -> [a] -> ([a], WonkyState)
bubblesortSinglePass _ wonkySt x [] = ([x], wonkySt)
bubblesortSinglePass lessThan wonkySt x (y : remaningElements) = do
  let (result, wonkySt') = lessThan x y wonkySt
  if result
    then do
      let (result', wonkySt'') = bubblesortSinglePass lessThan wonkySt' y remaningElements
      (x : result', wonkySt'')
    else do
      let (result', wonkySt'') = bubblesortSinglePass lessThan wonkySt' x remaningElements
      (y : result', wonkySt'')
