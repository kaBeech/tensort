module Data.Tensort.Utils.MkTsProps (mkTsProps) where

import Data.Tensort.Utils.Types (SortAlg, TensortProps (..))

mkTsProps :: Int -> SortAlg -> TensortProps
mkTsProps bSize subAlg = TensortProps {bytesize = bSize, subAlgorithm = subAlg}
