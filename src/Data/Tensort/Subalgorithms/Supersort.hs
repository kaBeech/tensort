module Data.Tensort.Subalgorithms.Supersort
  ( supersort,
    mundaneSuperStrat,
    magicSuperStrat,
  )
where

import Data.Tensort.Utils.Types (SortAlg, Sortable (..), SupersortStrat, WonkyState)

supersort :: Sortable -> (SortAlg, SortAlg, SortAlg, SupersortStrat) -> WonkyState -> (Sortable, WonkyState)
supersort xs (subAlg1, subAlg2, subAlg3, superStrat) wonkySt = do
  let (result1, _) = subAlg1 xs wonkySt
  let (result2, wonkySt') = subAlg2 xs wonkySt
  if result1 == result2
    then (result1, wonkySt')
    else do
      let (result3, wonkySt'') = subAlg3 xs wonkySt'
      (superStrat (result1, result2, result3), wonkySt'')

mundaneSuperStrat :: SupersortStrat
mundaneSuperStrat (SortBit result1, SortBit result2, SortBit result3) = do
  if result1 == result3 || result2 == result3
    then SortBit result3
    else
      if last result1 == last result2 || last result1 == last result3
        then SortBit result1
        else
          if last result2 == last result3
            then SortBit result2
            else SortBit result1
mundaneSuperStrat (SortRec result1, SortRec result2, SortRec result3) = do
  if result1 == result3 || result2 == result3
    then SortRec result3
    else
      if last result1 == last result2 || last result1 == last result3
        then SortRec result1
        else
          if last result2 == last result3
            then SortRec result2
            else SortRec result1
mundaneSuperStrat (_, _, _) = error "All three inputs must be of the same type."

magicSuperStrat :: SupersortStrat
magicSuperStrat (SortBit result1, SortBit result2, SortBit result3) = do
  if last result1 == last result3 || last result2 == last result3
    then SortBit result3
    else
      if last result1 == last result2
        then SortBit result1
        else SortBit result3
magicSuperStrat (SortRec result1, SortRec result2, SortRec result3) = do
  if last result1 == last result3 || last result2 == last result3
    then SortRec result3
    else
      if last result1 == last result2
        then SortRec result1
        else SortRec result3
magicSuperStrat (_, _, _) = error "All three inputs must be of the same type."
