-- | This module provides variations of the Robustsort algorithm
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
import Data.Tensort.Utils.LogNat (getLn, getLnLength)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.Types (Bit, SortAlg)

-- | Takes a list and returns a sorted list using a Basic Mundane
--   Robustsort algorithm with a Permutationsort adjudicator

-- | ==== __Examples__
-- >>> robustsortP ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> robustsortP ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
robustsortP :: (Ord a) => [Bit a] -> [Bit a]
robustsortP = tensort (mkTsProps 3 supersortP)

supersortP :: (Ord a) => [Bit a] -> [Bit a]
supersortP =
  supersort
    ( rotationsortReverse,
      bubblesort,
      permutationsort,
      mundaneSuperStrat
    )

-- | Takes a list and returns a sorted list using a Recursive Mundane
--   Robustsort algorithm with a Permutationsort adjudicator

-- | ==== __Examples__
--  >>> robustsortRP ([16, 23, 4, 8, 15, 42] :: [Int])
--  [4,8,15,16,23,42]
--
--  >>> robustsortRP ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
--  [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
robustsortRP :: (Ord a) => [Bit a] -> [Bit a]
robustsortRP = robustsortRCustom robustsortP

-- | Takes a list and returns a sorted list using a Basic Mundane
--   Robustsort algorithm with a Bogosort adjudicator

-- | ==== __Examples__
-- >>> robustsortB ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> robustsortB ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
robustsortB :: (Ord a) => [Bit a] -> [Bit a]
robustsortB = tensort (mkTsProps 3 supersortB)

supersortB :: (Ord a) => [Bit a] -> [Bit a]
supersortB =
  supersort
    ( rotationsortReverse,
      bubblesort,
      bogosort,
      mundaneSuperStrat
    )

-- | Takes a list and returns a sorted list using a Recursive Mundane
--   Robustsort algorithm with a Bogosort adjudicator

-- | ==== __Examples__
-- >>> robustsortRB ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> robustsortRB ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
robustsortRB :: (Ord a) => [Bit a] -> [Bit a]
robustsortRB = robustsortRCustom robustsortB

-- | Takes a list and returns a sorted list using a Basic Magic
--   Robustsort algorithm

-- | ==== __Examples__
-- >>> robustsortM ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> robustsortM ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
robustsortM :: (Ord a) => [Bit a] -> [Bit a]
robustsortM = tensort (mkTsProps 3 supersortM)

supersortM :: (Ord a) => [Bit a] -> [Bit a]
supersortM =
  supersort
    ( rotationsortAmbi,
      rotationsortReverseAmbi,
      magicsort,
      magicSuperStrat
    )

-- | Takes a list and returns a sorted list using a Recursive Magic
--   Robustsort algorithm

-- | ==== __Examples__
-- >>> robustsortRM ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> robustsortRM ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
robustsortRM :: (Ord a) => [Bit a] -> [Bit a]
robustsortRM = robustsortRCustom robustsortM

-- | Used for making recursive Robustsort variants
--
--   Takes the base SortAlg you want to use and a list and returns a sorted
--   list.
--
--   Uses a Logarithmic bytesize to determine when to stop recursing and use
--   the base SortAlg to sort the records.
--
--   Uses the base SortAlg once the bytesize is less than or equal to 27. This
--   number is chosen because the natural logarithm of 27 is close to 3 (it's
--   about 3.3) and the cube root of 27 is 3, so it's likely to be an efficient
--   choice.
--
--   This configuration is tailored to using a standard basic Robustsort
--   algorithm (i.e. with a Bytesize of 3) as the base SortAlg. You're welcome
--   to experiment with weirder setups too!
--
-- ==== __Examples__
-- >>> robustsortRCustom robustsortB ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> robustsortRCustom robustsortB ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
robustsortRCustom :: (Ord a) => SortAlg (Bit a) -> [Bit a] -> [Bit a]
robustsortRCustom baseSortAlg xs = tensort tsProps xs
  where
    tsProps = mkTsProps bytesize subAlg
    bytesize = getLnLength xs
    subAlg = robustsortRecursive bytesize baseSortAlg

-- | Used to create SubAlgorithms for use in recursive Robustsort variants. See
--   also `robustsortRCustom`.
--
--   Creates an algorithm that recursively applies Tensort with a Bytesize that
--   approximates the natural logarithm of the length of the input list until
--   the Bytesize is less than or equal to 27. At this point, the baseSortAlg
--   is used to sort the records.
robustsortRecursive :: (Ord a) => Int -> SortAlg (Bit a) -> SortAlg (Bit a)
robustsortRecursive bytesize baseSortAlg
  -- ln (532048240602) ~= 27
  -- ln (27) ~= 3
  -- 3 ^ 3 = 27
  -- So this is saying, if we have a bitesize of 532,048,240,602 or less, use
  -- one more iteration of Tensort to sort the records. This last iteration
  -- will use the baseSortAlg (such as the basic version of Robustsort with
  -- bytesize of 3 used in this module) to sort its records.
  | bytesize <= 27 = baseSortAlg
  | otherwise = tensort tsProps
  where
    tsProps = mkTsProps bytesize' baseSortAlg'
    bytesize' = getLn bytesize
    baseSortAlg' = robustsortRecursive bytesize' baseSortAlg
