-- | This module provides the mkTsProps function for creating TensortProps.
module Data.Tensort.Utils.MkTsProps (mkTsProps) where

import Data.Tensort.Utils.Types (TensortProps (..))

-- | Wraps in integer Bytesize and a SubAlgorithm together as TensortProps.
mkTsProps :: (Ord a) => Int -> ([a] -> [a]) -> TensortProps
mkTsProps bSize subAlg = TensortProps {bytesize = bSize, subAlgorithm = subAlg}
