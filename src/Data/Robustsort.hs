-- | This module provides convenience functions that wraps common Robustsort
--   functions to sort lists of Bits without dealing with type conversion
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
import Data.Tensort.Utils.Types (Bit)

-- | Takes a list of Bits and returns a sorted list of Bits using a Basic
--   Mundane Robustsort algorithm with a Permutationsort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortP' function

-- | ==== __Examples__
--   >>> robustsortP [16, 23, 4, 8, 15, 42]
--   [4,8,15,16,23,42]
robustsortP :: [Bit] -> [Bit]
robustsortP = Data.Tensort.Robustsort.robustsortP

-- | Takes a list of Bits and returns a sorted list of Bits using a Basic
--   Mundane Robustsort algorithm with a Bogosort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortB' function

-- | ==== __Examples__
--  >>> robustsortB [16, 23, 4, 8, 15, 42]
--  [4,8,15,16,23,42]
robustsortB :: [Bit] -> [Bit]
robustsortB = Data.Tensort.Robustsort.robustsortB

-- | Takes a list of Bits and returns a sorted list of Bits using a Basic
--   Magic Robustsort algorithm
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortM' function

-- | ==== __Examples__
--  >>> robustsortM [16, 23, 4, 8, 15, 42]
--  [4,8,15,16,23,42]
robustsortM :: [Bit] -> [Bit]
robustsortM = Data.Tensort.Robustsort.robustsortM

-- | Takes a list of Bits and returns a sorted list of Bits using a Recursive
--   Mundane Robustsort algorithm with a Permutationsort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortRP' function

-- | ==== __Examples__
--  >>> robustsortRP [16, 23, 4, 8, 15, 42]
--  [4,8,15,16,23,42]
robustsortRP :: [Bit] -> [Bit]
robustsortRP = Data.Tensort.Robustsort.robustsortRP

-- | Takes a list of Bits and returns a sorted list of Bits using a Recursive
--  Mundane Robustsort algorithm with a Bogosort adjudicator
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortRB' function

--  | ==== __Examples__
--  >>> robustsortRB [16, 23, 4, 8, 15, 42]
--  [4,8,15,16,23,42]
robustsortRB :: [Bit] -> [Bit]
robustsortRB = Data.Tensort.Robustsort.robustsortRB

-- | Takes a list of Bits and returns a sorted list of Bits using a Recursive
--   Magic Robustsort algorithm
--
--   This is a convenience function that wraps the
--   'Data.Tensort.Robustsort.robustsortRM' function

--   | ==== __Examples__
--   >>> robustsortRM [16, 23, 4, 8, 15, 42]
--   [4,8,15,16,23,42]
robustsortRM :: [Bit] -> [Bit]
robustsortRM = Data.Tensort.Robustsort.robustsortRM
