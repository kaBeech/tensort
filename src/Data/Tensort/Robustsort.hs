module Data.Tensort.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
  )
where

import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Supersort (magicSuperStrat, mundaneSuperStrat, supersort)
import Data.Tensort.Tensort (tensort)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.Types (Bit, Sortable, WonkyState)

robustsortP :: [Bit] -> WonkyState -> ([Bit], WonkyState)
robustsortP xs wonkySt = tensort xs (mkTsProps 3 supersortP) wonkySt

supersortP :: Sortable -> WonkyState -> (Sortable, WonkyState)
supersortP xs wonkySt = supersort xs (bubblesort, exchangesort, permutationsort, mundaneSuperStrat) wonkySt

robustsortB :: [Bit] -> WonkyState -> ([Bit], WonkyState)
robustsortB xs wonkySt = tensort xs (mkTsProps 3 supersortB) wonkySt

supersortB :: Sortable -> WonkyState -> (Sortable, WonkyState)
supersortB xs wonkySt = supersort xs (bubblesort, exchangesort, bogosort, mundaneSuperStrat) wonkySt

robustsortM :: [Bit] -> WonkyState -> ([Bit], WonkyState)
robustsortM xs wonkySt = tensort xs (mkTsProps 3 supersortM) wonkySt

supersortM :: Sortable -> WonkyState -> (Sortable, WonkyState)
supersortM xs wonkySt = supersort xs (bubblesort, exchangesort, magicsort, magicSuperStrat) wonkySt
