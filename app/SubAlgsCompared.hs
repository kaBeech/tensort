module SubAlgsCompared (subAlgsCompared) where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRB,
    robustsortRM,
    robustsortRP,
    supersortB,
    supersortM,
    supersortP,
  )
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Rotationsort
  ( rotationsort,
    rotationsortAmbi,
    rotationsortReverse,
    rotationsortReverseAmbi,
  )
import Data.Tensort.Utils.Types (Sortable, WonkyState)

subAlgsCompared ::
  [(WonkyState -> Sortable -> (Sortable, WonkyState), String)]
subAlgsCompared =
  [ (bubblesort, "Bubblesort"),
    (exchangesort, "Exchangesort"),
    (rotationsort, "Rotationsort"),
    (rotationsortReverse, "RotationsortReverse"),
    (rotationsortAmbi, "RotationsortAmbi"),
    (rotationsortReverseAmbi, "RotationsortReverseAmbi"),
    (mergesort, "Mergesort"),
    (quicksort, "Quicksort"),
    (bogosort, "Bogosort"),
    (permutationsort, "Permutationsort"),
    (magicsort, "Magicsort")
    -- (supersortP, "SupersortP"),
    -- (supersortB, "SupersortB"),
    -- (supersortM, "SupersortM"),
    -- (robustsortP, "RobustsortP"),
    -- (robustsortB, "RobustsortB"),
    -- (robustsortM, "RobustsortM"),
    -- (robustsortRP, "RobustsortRP"),
    -- (robustsortRB, "RobustsortRB"),
    -- (robustsortRM, "RobustsortRM")
  ]
