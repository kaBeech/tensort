module Data.Robustsort.Robustsort (robustsortP) where

import Data.Robustsort.Bytesort (bytesort)
import Data.Robustsort.Subalgorithms.Bogosort (bogosort)
import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Subalgorithms.Permutationsort (permutationsort)
import Data.Robustsort.Subalgorithms.Supersort (mundaneSuperStrat, supersort)
import Data.Robustsort.Utils.Bytes (mkBSProps)
import Data.Robustsort.Utils.Types (Sortable)

robustsortP :: Sortable -> Sortable
robustsortP xs = bytesort xs (mkBSProps 3 supersortP)

supersortP :: Sortable -> Sortable
supersortP xs = supersort xs (bubblesort, bogosort, permutationsort, mundaneSuperStrat)
