module Data.Robustsort.Subalgorithms.Supersort (supersort) where

import Data.Robustsort.Utils.Types (SortAlg, Sortable (..), SupersortStrat)

supersort :: Sortable -> SortAlg -> SortAlg -> SortAlg -> SupersortStrat -> Sortable
supersort xs subAlg1 subAlg2 subAlg3 superStrat = do
  let result1 = subAlg1 xs
  let result2 = subAlg2 xs
  if result1 == result2
    then result1
    else superStrat (result1, result2, subAlg3 xs)
