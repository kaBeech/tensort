-- | This module provides common Robustsort functions defined without reference
--   to Bits
module Data.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
    robustsortRP,
    robustsortRB,
    robustsortRM,
  )
where

import qualified Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRB,
    robustsortRM,
    robustsortRP,
  )

-- | Takes a list of Bits and returns a sorted list of Bits using a Basic
--   Mundane Robustsort algorithm with a Permutationsort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortP' function

-- | ==== __Examples__
--   >>> robustsortP ([16, 23, 4, 8, 15, 42] :: [Int])
--   [4,8,15,16,23,42]
robustsortP :: (Ord a) => [a] -> [a]
robustsortP = Data.Tensort.Robustsort.robustsortP

-- | Takes a list of Bits and returns a sorted list of Bits using a Basic
--   Mundane Robustsort algorithm with a Bogosort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortB' function

-- | ==== __Examples__
--  >>> robustsortB ([16, 23, 4, 8, 15, 42] :: [Int])
--  [4,8,15,16,23,42]
robustsortB :: (Ord a) => [a] -> [a]
robustsortB = Data.Tensort.Robustsort.robustsortB

-- | Takes a list of Bits and returns a sorted list of Bits using a Basic
--   Magic Robustsort algorithm
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortM' function

-- | ==== __Examples__
--  >>> robustsortM ([16, 23, 4, 8, 15, 42] :: [Int])
--  [4,8,15,16,23,42]
robustsortM :: (Ord a) => [a] -> [a]
robustsortM = Data.Tensort.Robustsort.robustsortM

-- | Takes a list of Bits and returns a sorted list of Bits using a Recursive
--   Mundane Robustsort algorithm with a Permutationsort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortRP' function

-- | ==== __Examples__
--  >>> robustsortRP ([16, 23, 4, 8, 15, 42] :: [Int])
--  [4,8,15,16,23,42]
robustsortRP :: (Ord a) => [a] -> [a]
robustsortRP = Data.Tensort.Robustsort.robustsortRP

-- | Takes a list of Bits and returns a sorted list of Bits using a Recursive
--  Mundane Robustsort algorithm with a Bogosort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortRB' function

--  | ==== __Examples__
--  >>> robustsortRB ([16, 23, 4, 8, 15, 42] :: [Int])
--  [4,8,15,16,23,42]
robustsortRB :: (Ord a) => [a] -> [a]
robustsortRB = Data.Tensort.Robustsort.robustsortRB

-- | Takes a list of Bits and returns a sorted list of Bits using a Recursive
--   Magic Robustsort algorithm
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortRM' function

--   | ==== __Examples__
--   >>> robustsortRM ([16, 23, 4, 8, 15, 42] :: [Int])
--   [4,8,15,16,23,42]
robustsortRM :: (Ord a) => [a] -> [a]
robustsortRM = Data.Tensort.Robustsort.robustsortRM
