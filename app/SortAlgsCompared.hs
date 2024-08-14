module SortAlgsCompared (sortAlgsCompared) where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRM,
    supersortB,
    supersortM,
    supersortP,
  )
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Utils.Types (Sortable, WonkyState)

sortAlgsCompared ::
  [(WonkyState -> Sortable -> (Sortable, WonkyState), String)]
sortAlgsCompared =
  [ (bubblesort, "Bubblesort"),
    (exchangesort, "Exchangesort"),
    (mergesort, "Mergesort"),
    (quicksort, "Quicksort"),
    (bogosort, "Bogosort"),
    (permutationsort, "Permutationsort"),
    (magicsort, "Magicsort"),
    (supersortP, "SupersortP"),
    (supersortB, "SupersortB"),
    (supersortM, "SupersortM"),
    (robustsortP, "RobustsortP"),
    (robustsortB, "RobustsortB"),
    (robustsortM, "RobustsortM"),
    (robustsortRM, "RobustsortRM")
  ]
