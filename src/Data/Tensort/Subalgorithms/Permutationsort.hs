module Data.Tensort.Subalgorithms.Permutationsort (permutationsort) where

import Data.List (permutations)
import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..), WonkyState)

permutationsort :: WonkyState -> Sortable -> (Sortable, WonkyState)
permutationsort wonkySt (SortBit bits) = do
  let initPermutations = permutations bits
  let (result, wonkySt') = permutationsortBits initPermutations wonkySt
  if null result
    then (SortBit bits, wonkySt')
    else (SortBit result, wonkySt')
permutationsort wonkySt (SortRec recs) = do
  let initPermutations = permutations recs
  let (result, wonkySt') = permutationsortRecords initPermutations wonkySt
  if null result
    then (SortRec recs, wonkySt')
    else (SortRec result, wonkySt')

permutationsortBits :: [[Bit]] -> WonkyState -> ([Bit], WonkyState)
permutationsortBits [] wonkySt = ([], wonkySt)
permutationsortBits (bits : remainingPermutations) wonkySt = do
  let (result, wonkySt') = isSorted (SortBit bits) wonkySt
  if result
    then (bits, wonkySt')
    else permutationsortBits remainingPermutations wonkySt'

permutationsortRecords :: [[Record]] -> WonkyState -> ([Record], WonkyState)
permutationsortRecords [] wonkySt = ([], wonkySt)
permutationsortRecords (recs : remainingPermutations) wonkySt = do
  let (result, wonkySt') = isSorted (SortRec recs) wonkySt
  if result
    then (recs, wonkySt')
    else permutationsortRecords remainingPermutations wonkySt'
