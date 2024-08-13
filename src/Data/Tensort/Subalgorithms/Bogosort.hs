module Data.Tensort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

bogosort :: WonkyState -> Sortable -> (Sortable, WonkyState)
bogosort = bogosortSeeded 143

bogosortSeeded :: Int -> WonkyState -> Sortable -> (Sortable, WonkyState)
bogosortSeeded seed wonkySt xs = do
  let (result, wonkySt') = isSorted xs wonkySt
  if result
    then (xs, wonkySt')
    else bogosortSeeded (seed + 1) wonkySt' (randomizeList seed xs)
