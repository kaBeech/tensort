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
rawElemsToUniverses :: DimensionsortProps a -> Hyperverse a -> Hyperverse a
rawElemsToUniverses dsProps (Universe elems) = Megaverse sortedUniverses
  where
    universes = map Universe $ splitEvery (versesize dsProps) elems
    subAlg = subAlgorithm dsProps
    sortedUniverses = map subAlg universes
rawElemsToUniverses dsProps (Megaverse verses) = Megaverse sortedVerses
  where
    megaverses = map Megaverse $ splitEvery (versesize dsProps) verses
    subAlg = subAlgorithm dsProps
    sortedVerses = map subAlg megaverses
