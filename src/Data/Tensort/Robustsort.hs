module Data.Tensort.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
    robustsortRecursive,
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
import Data.Tensort.Utils.Types (SortAlg, Sortable)

robustsortP :: Sortable -> Sortable
robustsortP = tensort (mkTsProps 3 supersortP)

supersortP :: Sortable -> Sortable
supersortP xs = supersort xs (bubblesort, exchangesort, permutationsort, mundaneSuperStrat)

robustsortB :: Sortable -> Sortable
robustsortB = tensort (mkTsProps 3 supersortB)

supersortB :: Sortable -> Sortable
supersortB xs = supersort xs (bubblesort, exchangesort, bogosort, mundaneSuperStrat)

robustsortM :: Sortable -> Sortable
robustsortM = tensort (mkTsProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM xs = supersort xs (bubblesort, exchangesort, magicsort, magicSuperStrat)

getLn :: Int -> Int
getLn x = ceiling (log (fromIntegral x) :: Double)

robustsortRecursive :: Int -> SortAlg -> SortAlg
robustsortRecursive bytesize baseSortAlg
  | bytesize <= 27 = baseSortAlg
  | otherwise = tensort (mkTsProps (getLn bytesize) (robustsortRecursive (getLn bytesize) baseSortAlg))
