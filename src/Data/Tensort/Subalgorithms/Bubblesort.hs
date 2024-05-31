module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Record, Sortable (..), Bit)

bubblesort :: Sortable -> Sortable
bubblesort (SortBit bits) = SortBit (foldr acc [] bits)
  where
    acc :: Bit -> [Bit] -> [Bit]
    acc x xs = bubblesortSinglePass x xs lessThanBit
bubblesort (SortRec recs) = SortRec (foldr acc [] recs)
  where
    acc :: Record -> [Record] -> [Record]
    acc x xs = bubblesortSinglePass x xs lessThanRecord

bubblesortSinglePass :: a -> [a] -> (a -> a -> Bool) -> [a]
bubblesortSinglePass x [] _ = [x]
bubblesortSinglePass x (y : remaningElements) lessThan = do
  if lessThan x y
    then x : bubblesortSinglePass y remaningElements lessThan
    else y : bubblesortSinglePass x remaningElements lessThan
