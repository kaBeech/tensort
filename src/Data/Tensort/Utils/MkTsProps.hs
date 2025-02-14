-- | This module provides the mkTsProps function for creating TensortProps.
module Data.Tensort.Utils.MkTsProps (mkTsProps) where

import Data.Tensort.Utils.Types (Bit, Record, SortAlg, TensortProps (..))

-- | Wraps in integer Bytesize and a SubAlgorithm together as TensortProps.
mkTsProps :: (Ord a) => Int -> SortAlg (Bit a) -> SortAlg (Record a) -> TensortProps a
mkTsProps bSize subAlgB subAlgR = TensortProps {bytesize = bSize, subAlgorithmBits = subAlgB, subAlgorithmRecs = subAlgR}
