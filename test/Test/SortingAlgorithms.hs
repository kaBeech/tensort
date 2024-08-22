module Test.SortingAlgorithms
  ( sortingAlgorithmsSortable,
    sortingAlgorithmsSortableShort,
    sortingAlgorithmsSortableTiny,
    sortingAlgorithmsBits,
  )
where

import qualified Data.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRB,
    robustsortRM,
    robustsortRP,
  )
import qualified Data.Tensort (tensort)
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
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Rotationsort (rotationsort, rotationsortAmbi, rotationsortReverse, rotationsortReverseAmbi)
import Data.Tensort.Subalgorithms.Supersort
  ( magicSuperStrat,
    mundaneSuperStrat,
    supersort,
  )
import Data.Tensort.Tensort (tensort, tensortB4, tensortBL)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.Types (Bit, SortAlg, Sortable)

tensortCustomExample :: Sortable -> Sortable
tensortCustomExample = tensort (mkTsProps 8 mergesort)

supersortMundaneCustomExample :: Sortable -> Sortable
supersortMundaneCustomExample =
  supersort
    ( quicksort,
      magicsort,
      bubblesort,
      mundaneSuperStrat
    )

supersortMagicCustomExample :: Sortable -> Sortable
supersortMagicCustomExample =
  supersort
    ( bogosort,
      permutationsort,
      magicsort,
      magicSuperStrat
    )

robustsortMundaneCustomExample :: Sortable -> Sortable
robustsortMundaneCustomExample =
  tensort
    (mkTsProps 3 supersortMundaneCustomExample)

robustsortMagicCustomExample :: Sortable -> Sortable
robustsortMagicCustomExample =
  tensort
    (mkTsProps 3 supersortMagicCustomExample)

sortingAlgorithmsSortable :: [(SortAlg, String)]
sortingAlgorithmsSortable =
  [ (quicksort, "Quicksort"),
    (mergesort, "Mergesort"),
    (bubblesort, "Bubblesort"),
    (exchangesort, "Exchangesort"),
    (permutationsort, "Permutationsort"),
    (tensortBL, "Standard Logaritmic Tensort"),
    (tensortB4, "Standard 4-Bit Tensort"),
    (tensortCustomExample, "Standard Custom Bitsize Tensort"),
    (robustsortP, "Standard Mundane Robustsort with Permutationsort adjudicator"),
    (robustsortB, "Standard Mundane Robustsort with Bogosort adjudicator"),
    (robustsortM, "Magic Robustsort"),
    (robustsortMundaneCustomExample, "Custom Mundane Robustsort"),
    (robustsortMagicCustomExample, "Custom Magic Robustsort"),
    (robustsortRP, "Recursive Mundane Robustsort with Permutationsort adjudicator"),
    (robustsortRB, "Recursive Mundane Robustsort with Bogosort adjudicator"),
    (robustsortRM, "Recursive Magic Robustsort")
  ]

sortingAlgorithmsSortableShort :: [(SortAlg, String)]
sortingAlgorithmsSortableShort =
  [ (bogosort, "Bogosort"),
    (magicsort, "Magicsort"),
    (robustsortP, "Standard Mundane Robustsort with Permutationsort adjudicator"),
    (robustsortB, "Standard Mundane Robustsort with Bogosort adjudicator"),
    (robustsortM, "Magic Robustsort"),
    (supersortMundaneCustomExample, "Custom Mundane Supersort"),
    (supersortMagicCustomExample, "Custom Magic Supersort")
  ]

sortingAlgorithmsSortableTiny :: [(SortAlg, String)]
sortingAlgorithmsSortableTiny =
  [ (rotationsort, "Rotationsort"),
    (rotationsortReverse, "Reverse Rotationsort"),
    (rotationsortAmbi, "Ambidextrous Rotationsort"),
    (rotationsortReverseAmbi, "Reverse Ambidextrous Rotationsort")
  ]

sortingAlgorithmsBits :: [([Bit] -> [Bit], String)]
sortingAlgorithmsBits =
  [ (Data.Tensort.tensort, "Top-level Tensort"),
    ( Data.Robustsort.robustsortP,
      "Top-level Mundane Robustsort with Permutationsort adjudicator"
    ),
    ( Data.Robustsort.robustsortB,
      "Top-level Mundane Robustsort with Bogosort adjudicator"
    ),
    ( Data.Robustsort.robustsortM,
      "Top-level Magic Robustsort"
    ),
    ( Data.Robustsort.robustsortRP,
      "Top-level Recursive Mundane Robustsort with Permutationsort adjudicator"
    ),
    ( Data.Robustsort.robustsortRB,
      "Top-level Recursive Mundane Robustsort with Bogosort adjudicator"
    ),
    ( Data.Robustsort.robustsortRM,
      "Top-level Recursive Magic Robustsort"
    )
  ]
