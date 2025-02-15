-- | This module provides common Dimensionsort functions wrapped to sort lists
--   of Ords without dealing with type conversion
module Data.Dimensionsort
  ( dimensionsort,
  )
where

import Data.Tensort.Dimensionsort (dimensionsortBL)
import Data.Tensort.Dimensionsort.Types (sortAlgFromDimensional)

-- | Takes a list of Ords and returns a sorted list of Ords using a Standard
--   Logarithmic Dimensionsort algorithm
--
--   This is a convenience function that wraps the 'dimensionsortBL' function

-- | ==== __Examples__
--   >>> dimensionsort ([16, 23, 4, 8, 15, 42] :: [Int])
--   [4,8,15,16,23,42]
dimensionsort :: (Ord a) => [a] -> [a]
dimensionsort = sortAlgFromDimensional dimensionsortBL
