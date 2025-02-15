-- | This module provides the mkDsProps function for creating DimensionsortProps.
module Data.Tensort.Dimensionsort.MkDsProps (mkDsProps) where

import Data.Tensort.Dimensionsort.Types (DimensionsortProps (..), SortAlg)

-- | Wraps an integer Bytesize and a SubAlgorithm together as DimensionsortProps.
mkDsProps :: (Ord a) => Int -> SortAlg a -> DimensionsortProps a
mkDsProps vSize subAlg =
  DimensionsortProps
    { versesize = vSize,
      subAlgorithm = subAlg
    }
