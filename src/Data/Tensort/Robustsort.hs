module Data.Tensort.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
  )
where

import Data.Tensort.Tensort (tensort)
import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.ReverseExchangesort (reverseExchangesort)
import Data.Tensort.Subalgorithms.Supersort (magicSuperStrat, mundaneSuperStrat, supersort)
import Data.Tensort.Utils.Bytes (mkTSProps)
import Data.Tensort.Utils.Types (Sortable)

robustsortP :: Sortable -> Sortable
robustsortP xs = tensort xs (mkTSProps 3 supersortP)

supersortP :: Sortable -> Sortable
supersortP xs = supersort xs (bubblesort, reverseExchangesort, permutationsort, mundaneSuperStrat)

robustsortB :: Sortable -> Sortable
robustsortB xs = tensort xs (mkTSProps 3 supersortB)

supersortB :: Sortable -> Sortable
supersortB xs = supersort xs (bubblesort, reverseExchangesort, bogosort, mundaneSuperStrat)

robustsortM :: Sortable -> Sortable
robustsortM xs = tensort xs (mkTSProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM xs = supersort xs (bubblesort, reverseExchangesort, magicsort, magicSuperStrat)
