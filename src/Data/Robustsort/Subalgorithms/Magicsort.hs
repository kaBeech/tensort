module Data.Robustsort.Subalgorithms.Magicsort
  ( magicsort,
  )
where

import Data.Robustsort.Subalgorithms.Bogosort (bogosort)
import Data.Robustsort.Subalgorithms.Permutationsort (permutationsort)
import Data.Robustsort.Utils.Types (Sortable)

magicsort :: Sortable -> Sortable
magicsort xs = do
  let result1 = permutationsort xs
  let result2 = bogosort xs
  if result1 == result2
    then result1
    else magicsort xs
