module Data.Tensort.Utils.Check (isSorted) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanOrEqualBit, lessThanOrEqualRecord)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

isSorted :: Sortable -> WonkyState -> (Bool, WonkyState)
isSorted (SortBit []) wonkySt = (True, wonkySt)
isSorted (SortBit [_]) wonkySt = (True, wonkySt)
isSorted (SortBit (x : y : remainingElements)) wonkySt = do
  let (result, wonkySt') = lessThanOrEqualBit x y wonkySt
  if result
    then isSorted (SortBit (y : remainingElements)) wonkySt'
    else (False, wonkySt')
isSorted (SortRec []) wonkySt = (True, wonkySt)
isSorted (SortRec [_]) wonkySt = (True, wonkySt)
isSorted (SortRec (x : y : remainingElements)) wonkySt = do
  let (result, wonkySt') = lessThanOrEqualRecord x y wonkySt
  if result
    then isSorted (SortRec (y : remainingElements)) wonkySt'
    else (False, wonkySt')
