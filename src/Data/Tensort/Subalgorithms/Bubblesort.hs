module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..), WonkyState)

bubblesort :: Sortable -> WonkyState -> (Sortable, WonkyState)
bubblesort (SortBit bits) wonkySt = do
  let (result, wonkySt') = foldr acc ([], wonkySt) bits
  (SortBit result, wonkySt')
  where
    acc :: Bit -> ([Bit], WonkyState) -> ([Bit], WonkyState)
    acc x (xs, wonkySt') = bubblesortSinglePass x xs lessThanBit wonkySt'
bubblesort (SortRec recs) wonkySt = do
  let (result, wonkySt') = foldr acc ([], wonkySt) recs
  (SortRec result, wonkySt')
  where
    acc :: Record -> ([Record], WonkyState) -> ([Record], WonkyState)
    acc x (xs, wonkySt') = bubblesortSinglePass x xs lessThanRecord wonkySt'

bubblesortSinglePass :: a -> [a] -> (a -> a -> WonkyState -> (Bool, WonkyState)) -> WonkyState -> ([a], WonkyState)
bubblesortSinglePass x [] _ wonkySt = ([x], wonkySt)
bubblesortSinglePass x (y : remaningElements) lessThan wonkySt = do
  let (result, wonkySt') = lessThan x y wonkySt
  if result
    then do
      let (result', wonkySt'') = bubblesortSinglePass y remaningElements lessThan wonkySt'
      (x : result', wonkySt'')
    else do
      let (result', wonkySt'') = bubblesortSinglePass x remaningElements lessThan wonkySt'
      (y : result', wonkySt'')
