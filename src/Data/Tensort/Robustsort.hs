module Data.Tensort.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
  )
where

import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.ReverseExchangesort (reverseExchangesort)
import Data.Tensort.Subalgorithms.Supersort (magicSuperStrat, mundaneSuperStrat, supersort)
import Data.Tensort.Tensort (mkTSProps, tensort)
import Data.Tensort.Utils.Types (Sortable)

robustsortP :: [Int] -> [Int]
robustsortP xs = tensort xs (mkTSProps 3 supersortP)

supersortP :: Sortable -> Sortable
supersortP xs = supersort xs (bubblesort, reverseExchangesort, permutationsort, mundaneSuperStrat)

robustsortB :: [Int] -> [Int]
robustsortB xs = tensort xs (mkTSProps 3 supersortB)

supersortB :: Sortable -> Sortable
supersortB xs = supersort xs (bubblesort, reverseExchangesort, bogosort, mundaneSuperStrat)

robustsortM :: [Int] -> [Int]
robustsortM xs = tensort xs (mkTSProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM xs = supersort xs (bubblesort, reverseExchangesort, magicsort, magicSuperStrat)
