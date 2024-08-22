-- | This module provides variations of the Robustsort algorithm using the
--   Sortable type
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
import Data.Tensort.Subalgorithms.Magicsort (magicsort)
import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
import Data.Tensort.Subalgorithms.Rotationsort
  ( rotationsortAmbi,
    rotationsortReverse,
    rotationsortReverseAmbi,
  )
import Data.Tensort.Subalgorithms.Supersort
  ( magicSuperStrat,
    mundaneSuperStrat,
    supersort,
  )
import Data.Tensort.Tensort (tensort)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.Types (SortAlg, Sortable (..), WonkyState, fromSortBit)

-- | Takes a Sortable and returns a sorted Sortable using a Recursive Mundane
--   Robustsort algorithm with a Permutationsort adjudicator

-- | ==== __Examples__
--  >>> robustsortRP (SortBit [16, 23, 4, 8, 15, 42])
--  SortBit [4,8,15,16,23,42]
robustsortRP :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortRP = robustsortRCustom robustsortP

robustsortP :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortP = tensort (mkTsProps 3 supersortP)

supersortP :: WonkyState -> Sortable -> (Sortable, WonkyState)
supersortP =
  supersort
    ( rotationsortReverse,
      bubblesort,
      permutationsort,
      mundaneSuperStrat
    )

-- | Takes a Sortable and returns a sorted Sortable using a Recursive Mundane
--   Robustsort algorithm with a Bogosort adjudicator

-- | ==== __Examples__
-- >>> robustsortRB (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
robustsortRB :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortRB = robustsortRCustom robustsortB

robustsortB :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortB = tensort (mkTsProps 3 supersortB)

supersortB :: WonkyState -> Sortable -> (Sortable, WonkyState)
supersortB =
  supersort
    ( rotationsortReverse,
      bubblesort,
      bogosort,
      mundaneSuperStrat
    )

-- | Takes a Sortable and returns a sorted Sortable using a Recursive Magic
--   Robustsort algorithm

-- | ==== __Examples__
-- >>> robustsortRM (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
robustsortRM :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortRM = robustsortRCustom robustsortM

robustsortM :: WonkyState -> Sortable -> (Sortable, WonkyState)
robustsortM = tensort (mkTsProps 3 supersortM)

supersortM :: WonkyState -> Sortable -> (Sortable, WonkyState)
supersortM =
  supersort
    ( rotationsortAmbi,
      rotationsortReverseAmbi,
      magicsort,
      magicSuperStrat
    )

-- | Used for making recursive Robustsort algorithms
robustsortRCustom ::
  SortAlg ->
  WonkyState ->
  Sortable ->
  (Sortable, WonkyState)
robustsortRCustom baseSortAlg wonkySt xs =
  tensort
    ( mkTsProps
        (getLnBytesize xs)
        (robustsortRecursive (getLnBytesize xs) baseSortAlg)
    )
    wonkySt
    xs

getLnBytesize :: Sortable -> Int
getLnBytesize (SortBit xs) = getLn (length xs)
getLnBytesize (SortRec xs) = getLn (length xs)

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
  | otherwise =
      tensort
        ( mkTsProps
            (getLn bytesize)
            (robustsortRecursive (getLn bytesize) baseSortAlg)
        )
