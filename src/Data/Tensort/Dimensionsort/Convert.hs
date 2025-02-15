module Data.Tensort.Dimensionsort.Convert (rawElemsToUniverses) where

import Data.Tensort.Dimensionsort.Types
  ( DimensionsortProps (..),
    Hyperverse (..),
  )
import Data.Tensort.Utils.Split (splitEvery)

-- | Convert a list of Elems to a list of Universes of given versesize, sorting
--   each Universe with the given subalgorithm.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Dimensionsort.MkDsProps (mkDsProps)
--   >>> rawElemsToUniverses (mkDsProps 4 bubblesort) ([5,1,3,7,8,2,4,6] :: [Int])
--   [[2,4,6,8],[1,3,5,7]]
rawElemsToUniverses :: DimensionsortProps a -> Hyperverse a -> [Hyperverse a]
rawElemsToUniverses dsProps (Universe elems) = foldr acc [] universes
  where
    universes = map Universe $ splitEvery (versesize dsProps) elems
    acc universe universesSorted =
      universesSorted ++ [universeSorted]
      where
        universeSorted = subAlgorithm dsProps universe
rawElemsToUniverses _ (Megaverse _) = error "rawElemsToUniverses: Megaverse input not supported"
