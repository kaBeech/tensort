-- | This module provides convenience functions that wraps common Tensort
--   functions to sort lists of Bits without dealing with type conversion
module Data.Tensort
  ( tensort,
  )
where

import Data.Tensort.Tensort (tensortBL)
import Data.Tensort.Utils.Types (Bit)

-- | Takes a list of Bits and returns a sorted list of Bits using a Standard
--   Logarithmic Tensort algorithm
--
--   This is a convenience function that wraps the 'tensortBL' function

-- | ==== __Examples__
--   >>> tensort [16, 23, 4, 8, 15, 42]
--   [4,8,15,16,23,42]
tensort :: (Ord a) => [Bit a] -> [Bit a]
tensort = tensortBL
