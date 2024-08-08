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
import Data.Tensort.Utils.Types (Bit, Sortable)

robustsortP :: [Bit] -> [Bit]
robustsortP xs = tensort xs (mkTsProps 3 supersortP)

supersortP :: Sortable -> Sortable
supersortP xs = supersort xs (bubblesort, exchangesort, permutationsort, mundaneSuperStrat)

robustsortB :: [Bit] -> [Bit]
robustsortB xs = tensort xs (mkTsProps 3 supersortB)

supersortB :: Sortable -> Sortable
supersortB xs = supersort xs (bubblesort, exchangesort, bogosort, mundaneSuperStrat)

robustsortM :: [Bit] -> [Bit]
robustsortM xs = tensort xs (mkTsProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM xs = supersort xs (bubblesort, exchangesort, magicsort, magicSuperStrat)

-- robustsortCustomRecursive :: [Bit] -> Int -> SortAlg -> [Bit]
-- robustsortCustomRecursive xs currentBytesize baseSortAlg =
--   if currentBytesize <= 27
--     then baseSortAlg xs
--     else do
--       let currentBytesize' = ceiling (log (fromIntegral currentBytesize) :: Double)
--       tensort xs (mkTsProps currentBytesize' (robustsortCustomRecursive xs currentBytesize' baseSortAlg))
