-- | This module provides the mkTsProps function for creating TensortProps
module Data.Tensort.Utils.MkTsProps (mkTsProps) where

import Data.Tensort.Utils.Types (SortAlg, TensortProps (..))

-- | Wraps in integer Bytesize and a SortAlg together as TensortProps
mkTsProps :: Int -> SortAlg -> TensortProps
mkTsProps bSize subAlg = TensortProps {bytesize = bSize, subAlgorithm = subAlg}
