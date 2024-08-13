module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..))

bubblesort :: Sortable -> Sortable
bubblesort (SortBit bits) = SortBit (foldr acc [] bits)
  where
    acc :: Bit -> [Bit] -> [Bit]
    acc = bubblesortSinglePass lessThanBit
bubblesort (SortRec recs) = SortRec (foldr acc [] recs)
  where
    acc :: Record -> [Record] -> [Record]
    acc = bubblesortSinglePass lessThanRecord

bubblesortSinglePass :: (a -> a -> Bool) -> a -> [a] -> [a]
bubblesortSinglePass _ x [] = [x]
bubblesortSinglePass lessThan x (y : remaningElements) =
  if lessThan x y
    then x : bubblesortSinglePass lessThan y remaningElements
    else y : bubblesortSinglePass lessThan x remaningElements
