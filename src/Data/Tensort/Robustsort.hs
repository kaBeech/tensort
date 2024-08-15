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
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Rotationsort
  ( rotationsort,
    rotationsortAmbi,
    rotationsortReverseAmbi,
  )
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
    ( rotationsort,
      bubblesort,
      permutationsort,
      mundaneSuperStrat
    )

robustsortRB :: Sortable -> Sortable
robustsortRB = robustsortRCustom robustsortB

robustsortB :: Sortable -> Sortable
robustsortB = tensort (mkTsProps 3 supersortB)

supersortB :: Sortable -> Sortable
supersortB = supersort (rotationsort, bubblesort, bogosort, mundaneSuperStrat)

robustsortRM :: Sortable -> Sortable
robustsortRM = robustsortRCustom robustsortM

robustsortM :: Sortable -> Sortable
robustsortM = tensort (mkTsProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM = supersort (rotationsortAmbi, rotationsortReverseAmbi, magicsort, magicSuperStrat)

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
  -- ln (532048240602) ~= 27
  -- ln (27) ~= 3
  -- 3 ^ 3 = 27
  -- So this is saying, if we have a bitesize of 532,048,240,602 or less, use
  -- one more iteration of Tensort to sort the records. This last iteration
  -- will use the baseSortAlg (which by default is a standard version of
  -- Robustsort with a bytesize of 3) to sort its records.
  | bytesize <= 27 = baseSortAlg
  | otherwise = tensort (mkTsProps (getLn bytesize) (robustsortRecursive (getLn bytesize) baseSortAlg))
