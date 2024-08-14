module Data.Tensort.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
    supersortP,
    supersortB,
    supersortM,
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
import Data.Tensort.Utils.Types (SortAlg, Sortable, WonkyState, fromSortBit)

robustsortRP :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortRP = robustsortRCustom robustsortP

robustsortP :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortP = tensort (mkTsProps 3 supersortP)

supersortP :: WonkyState -> Sortable -> (Sortable, WonkyState)
supersortP =
  supersort
    ( bubblesort,
      exchangesort,
      permutationsort,
      mundaneSuperStrat
    )

robustsortRB :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortRB = robustsortRCustom robustsortB

robustsortB :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortB = tensort (mkTsProps 3 supersortB)

supersortB :: WonkyState -> Sortable -> (Sortable, WonkyState)
supersortB = supersort (bubblesort, exchangesort, bogosort, mundaneSuperStrat)

robustsortRM :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortRM = robustsortRCustom robustsortM

robustsortM :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortM = tensort (mkTsProps 3 supersortM)

supersortM :: WonkyState -> Sortable -> (Sortable, WonkyState)
supersortM = supersort (bubblesort, exchangesort, magicsort, magicSuperStrat)

robustsortRCustom :: SortAlg -> WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortRCustom baseSortAlg wonkySt xs =
  tensort
    ( mkTsProps
        (getLnBytesize xs)
        (robustsortRecursive (getLnBytesize xs) baseSortAlg)
    )
    wonkySt
    xs

getLnBytesize :: Sortable -> Int
getLnBytesize xs = getLn (length (fromSortBit xs))

getLn :: Int -> Int
getLn x = ceiling (log (fromIntegral x) :: Double)

robustsortRecursive :: Int -> SortAlg -> SortAlg
robustsortRecursive bytesize baseSortAlg
  -- ln (532048240602) ~= 27
  -- ln (27) ~= 3
  -- 3 ^ 3 = 27
  -- So this is saying, if we have a bitesize of 532,048,240,602 or less, use
  -- one more iteration of Tensort to sort the records. This last iteration
  -- will use the baseSortAlg (which by default is a standard version of
  -- Robustsort with a bytesize of 3) to sort its records.
  | bytesize <= 532048240602 = tensort (mkTsProps (getLn bytesize) baseSortAlg)
  | otherwise = tensort (mkTsProps (getLn bytesize) (robustsortRecursive (getLn bytesize) baseSortAlg))
