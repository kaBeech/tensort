module Data.Robustsort.Subalgorithms.Supersort
  ( supersort,
    mundaneSuperStrat,
    magicSuperStrat,
  )
where

import Data.Robustsort.Utils.Types (SortAlg, Sortable (..), SupersortStrat)

supersort :: Sortable -> SortAlg -> SortAlg -> SortAlg -> SupersortStrat -> Sortable
supersort xs subAlg1 subAlg2 subAlg3 superStrat = do
  let result1 = subAlg1 xs
  let result2 = subAlg2 xs
  if result1 == result2
    then result1
    else superStrat (result1, result2, subAlg3 xs)

mundaneSuperStrat :: SupersortStrat
mundaneSuperStrat (SortInt result1, SortInt result2, SortInt result3) = do
  if result1 == result3 || result2 == result3
    then SortInt result3
    else
      if last result1 == last result2 || last result1 == last result3
        then SortInt result1
        else
          if last result2 == last result3
            then SortInt result2
            else SortInt result1
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
magicSuperStrat (SortInt result1, SortInt result2, SortInt result3) = do
  if last result1 == last result3 || last result2 == last result3
    then SortInt result3
    else
      if last result1 == last result2
        then SortInt result1
        else SortInt result3
magicSuperStrat (SortRec result1, SortRec result2, SortRec result3) = do
  if last result1 == last result3 || last result2 == last result3
    then SortRec result3
    else
      if last result1 == last result2
        then SortRec result1
        else SortRec result3
magicSuperStrat (_, _, _) = error "All three inputs must be of the same type."
