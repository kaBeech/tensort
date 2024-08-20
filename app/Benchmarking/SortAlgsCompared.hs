module Benchmarking.SortAlgsCompared (sortAlgsCompared) where

import Data.Tensort.OtherSorts.Mergesort (mergesort)
import Data.Tensort.OtherSorts.Quicksort (quicksort)
import Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRB,
    robustsortRM,
    robustsortRP,
  )
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Tensort (tensortBL)
import Data.Tensort.Utils.Types (Sortable, WonkyState)

sortAlgsCompared ::
  [(WonkyState -> Sortable -> (Sortable, WonkyState), String)]
sortAlgsCompared =
  [ (robustsortRM, "Magic Robustsort"),
    (robustsortRB, "Bogo Robustsort"),
    (robustsortRP, "Mundane Robustsort"),
    (robustsortM, "Base RobustsortM"),
    (robustsortB, "Base RobustsortB"),
    (robustsortP, "Base RobustsortP"),
    (tensortBL, "TensortBL"),
    (bubblesort, "Bubblesort"),
    (quicksort, "Quicksort"),
    (mergesort, "Mergesort")
  ]
