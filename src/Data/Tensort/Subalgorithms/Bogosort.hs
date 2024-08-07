module Data.Tensort.Subalgorithms.Bogosort (bogosort, bogosortSeeded) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

bogosort :: Sortable -> WonkyState -> (Sortable, WonkyState)
bogosort xs = bogosortSeeded xs 143

bogosortSeeded :: Sortable -> Int -> WonkyState -> (Sortable, WonkyState)
bogosortSeeded xs seed wonkySt = do
  let (result, wonkySt') = isSorted xs wonkySt
  if result
    then (xs, wonkySt')
    else bogosortSeeded (randomizeList xs seed) (seed + 1) wonkySt'
