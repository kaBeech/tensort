-- | This module provides variations of the Robustsort algorithm using the
--   custom Sortable type for inputs and outputs
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
import Data.Tensort.Utils.LogNat (getLn, getLnBytesize)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.Types (SortAlg, Sortable (..))

-- | Takes a Sortable and returns a sorted Sortable using a Recursive Mundane
--   Robustsort algorithm with a Permutationsort adjudicator

-- | ==== __Examples__
--  >>> robustsortRP (SortBit [16, 23, 4, 8, 15, 42])
--  SortBit [4,8,15,16,23,42]
--
--  >>> robustsortRP (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
--  SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
robustsortRP :: Sortable -> Sortable
robustsortRP = robustsortRCustom robustsortP

-- | Takes a Sortable and returns a sorted Sortable using a Basic Mundane
--   Robustsort algorithm with a Permutationsort adjudicator

-- | ==== __Examples__
-- >>> robustsortP (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> robustsortP (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
robustsortP :: Sortable -> Sortable
robustsortP = tensort (mkTsProps 3 supersortP)

supersortP :: Sortable -> Sortable
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
--
-- >>> robustsortRB (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
robustsortRB :: Sortable -> Sortable
robustsortRB = robustsortRCustom robustsortB

-- | Takes a Sortable and returns a sorted Sortable using a Basic Mundane
--   Robustsort algorithm with a Bogosort adjudicator

-- | ==== __Examples__
-- >>> robustsortB (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> robustsortB (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
robustsortB :: Sortable -> Sortable
robustsortB = tensort (mkTsProps 3 supersortB)

supersortB :: Sortable -> Sortable
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
--
-- >>> robustsortRM (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
robustsortRM :: Sortable -> Sortable
robustsortRM = robustsortRCustom robustsortM

-- | Takes a Sortable and returns a sorted Sortable using a Basic Magic
--   Robustsort algorithm

-- | ==== __Examples__
-- >>> robustsortM (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> robustsortM (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
robustsortM :: Sortable -> Sortable
robustsortM = tensort (mkTsProps 3 supersortM)

supersortM :: Sortable -> Sortable
supersortM =
  supersort
    ( rotationsortAmbi,
      rotationsortReverseAmbi,
      magicsort,
      magicSuperStrat
    )

-- | Used for making recursive Robustsort variants
--
--   Takes the base SortAlg you want to use and a Sortable and returns a sorted
--   Sortable.
--
--   Uses a Logarithmic bytesize to determine when to stop recursing and use
--   the base SortAlg to sort the records.
--
--   Uses the base SortAlg once the bytesize is less than or equal to 27. This
--   number is chosen because it is the natural logarithm of 27 is close to
--   3 (it's abuot 3.3) and the square root of 27 is 3, so it's likely to be an
--   efficient choice.
--
--   This confiuguration is tailored to using a standard basic Robustsort
--   algorithm (i.e. with a Bytesize of 3) as the base SortAlg. You're welcome
--   to experiment with weirder setups too!
--
-- ==== __Examples__
-- >>> robustsortRCustom robustsortB (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> robustsortRCustom robustsortB (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
robustsortRCustom :: SortAlg -> Sortable -> Sortable
robustsortRCustom baseSortAlg xs =
  tensort
    ( mkTsProps
        (getLnBytesize xs)
        (robustsortRecursive (getLnBytesize xs) baseSortAlg)
    )
    xs

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
