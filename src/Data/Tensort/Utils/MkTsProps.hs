-- | This module provides the mkTsProps function for creating TensortProps.
module Data.Tensort.Utils.MkTsProps (mkTsProps) where

import Data.Tensort.Utils.Types (SortAlg, TensortProps (..))

-- | Wraps an integer Bytesize and a SubAlgorithm together as TensortProps.
mkTsProps :: (Ord a) => Int -> SortAlg a -> TensortProps a
mkTsProps bSize subAlg = TensortProps {bytesize = bSize, subAlgorithm = subAlg}
