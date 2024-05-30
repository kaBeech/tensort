module Data.Tensort.Subalgorithms.Magicsort
  ( magicsort,
  )
where

import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Utils.Types (Sortable)

magicsort :: Sortable -> Sortable
magicsort xs = do
  let result1 = permutationsort xs
  let result2 = bogosort xs
  if result1 == result2
    then result1
    else magicsort xs
