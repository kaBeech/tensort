module Data.Tensort.Subalgorithms.Magicsort
  ( magicsort,
  )
where

import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

magicsort :: Sortable -> WonkyState -> (Sortable, WonkyState)
magicsort xs wonkySt = do
  let (result1, _) = permutationsort xs wonkySt
  let (result2, wonkySt') = bogosort xs wonkySt
  if verifyResults result1 result2
    then (result1, wonkySt')
    else magicsort xs wonkySt'

verifyResults :: Sortable -> Sortable -> Bool
verifyResults (SortBit xs) (SortBit ys) = xs == ys
verifyResults (SortRec xs) (SortRec ys) = map snd xs == map snd ys
verifyResults (SortBit _) (SortRec _) = False
verifyResults (SortRec _) (SortBit _) = False
