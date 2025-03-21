-- | This module provides common Tensort functions defined without reference to
--   Bits
module Data.Tensort
  ( tensort,
  )
where

import Data.Tensort.Tensort (tensortBL)

-- | Takes a list of Bits and returns a sorted list of Bits using a Standard
--   Logarithmic Tensort algorithm
--
--   This is a convenience function that wraps the 'tensortBL' function

-- | ==== __Examples__
--   >>> tensort ([16, 23, 4, 8, 15, 42] :: [Int])
--   [4,8,15,16,23,42]
tensort :: (Ord a) => [a] -> [a]
tensort = tensortBL
