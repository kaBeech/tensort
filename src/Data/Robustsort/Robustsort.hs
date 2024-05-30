module Data.Robustsort.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
  )
where

import Data.Robustsort.Bytesort (bytesort)
import Data.Robustsort.Subalgorithms.Bogosort (bogosort)
import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Subalgorithms.Magicsort (magicsort)
import Data.Robustsort.Subalgorithms.Permutationsort (permutationsort)
import Data.Robustsort.Subalgorithms.ReverseExchangesort (reverseExchangesort)
import Data.Robustsort.Subalgorithms.Supersort (magicSuperStrat, mundaneSuperStrat, supersort)
import Data.Robustsort.Utils.Bytes (mkBSProps)
import Data.Robustsort.Utils.Types (Sortable)

robustsortP :: Sortable -> Sortable
robustsortP xs = bytesort xs (mkBSProps 3 supersortP)

supersortP :: Sortable -> Sortable
supersortP xs = supersort xs (bubblesort, reverseExchangesort, permutationsort, mundaneSuperStrat)

robustsortB :: Sortable -> Sortable
robustsortB xs = bytesort xs (mkBSProps 3 supersortB)

supersortB :: Sortable -> Sortable
supersortB xs = supersort xs (bubblesort, reverseExchangesort, bogosort, mundaneSuperStrat)

robustsortM :: Sortable -> Sortable
robustsortM xs = bytesort xs (mkBSProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM xs = supersort xs (bubblesort, reverseExchangesort, magicsort, magicSuperStrat)
