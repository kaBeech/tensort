module Data.Tensort.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
    robustsortRCustom,
    robustsortRP,
    robustsortRB,
    robustsortRM,
  )
where

import Data.Tensort.Subalgorithms.Bogosort (bogosort)
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Subalgorithms.Exchangesort (exchangesort)
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Supersort
  ( magicSuperStrat,
    mundaneSuperStrat,
    supersort,
  )
import Data.Tensort.Tensort (tensort)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.Types (SortAlg, Sortable, fromSortBit)

robustsortRP :: Sortable -> Sortable
robustsortRP = robustsortRCustom robustsortP

robustsortP :: Sortable -> Sortable
robustsortP = tensort (mkTsProps 3 supersortP)

supersortP :: Sortable -> Sortable
supersortP =
  supersort
    ( bubblesort,
      exchangesort,
      permutationsort,
      mundaneSuperStrat
    )

robustsortRB :: Sortable -> Sortable
robustsortRB = robustsortRCustom robustsortB

robustsortB :: Sortable -> Sortable
robustsortB = tensort (mkTsProps 3 supersortB)

supersortB :: Sortable -> Sortable
supersortB = supersort (bubblesort, exchangesort, bogosort, mundaneSuperStrat)

robustsortRM :: Sortable -> Sortable
robustsortRM = robustsortRCustom robustsortM

robustsortM :: Sortable -> Sortable
robustsortM = tensort (mkTsProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM = supersort (bubblesort, exchangesort, magicsort, magicSuperStrat)

robustsortRCustom :: SortAlg -> Sortable -> Sortable
robustsortRCustom baseSortAlg xs =
  tensort
    ( mkTsProps
        (getLnBytesize xs)
        (robustsortRecursive (getLnBytesize xs) baseSortAlg)
    )
    xs

getLnBytesize :: Sortable -> Int
getLnBytesize xs = getLn (length (fromSortBit xs))

getLn :: Int -> Int
getLn x = ceiling (log (fromIntegral x) :: Double)

robustsortRecursive :: Int -> SortAlg -> SortAlg
robustsortRecursive bytesize baseSortAlg
  | bytesize <= 528491359 = tensort (mkTsProps (getLn bytesize) (baseSortAlg))
  | otherwise = tensort (mkTsProps (getLn bytesize) (robustsortRecursive (getLn bytesize) baseSortAlg))
