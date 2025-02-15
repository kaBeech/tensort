module Data.Tensort.Dimensionsort.Convert (condenseHyperverses, condenseMegaverse) where

import Data.Tensort.Dimensionsort.Types
  ( DimensionsortProps (..),
    Hyperverse (..),
  )
import Data.Tensort.Utils.Split (splitEvery)

condenseMegaverse :: (Eq a, Show a, Ord a) => DimensionsortProps a -> Hyperverse a -> Hyperverse a
condenseMegaverse dsProps (Universe megaverse) =
  if length megaverse == 1
    then Universe megaverse
    else condenseMegaverse dsProps $ condenseHyperverses dsProps $ Universe megaverse
condenseMegaverse dsProps (Megaverse megaverse) =
  if length megaverse == 1
    then head megaverse
    else condenseMegaverse dsProps $ condenseHyperverses dsProps $ Megaverse megaverse

-- | Convert a list of Elems to a list of Universes of given versesize, sorting
--   each Universe with the given subalgorithm.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Dimensionsort.MkDsProps (mkDsProps)
--   >>> condenseHyperverses (mkDsProps 4 bubblesort) ([5,1,3,7,8,2,4,6] :: [Int])
--   [[2,4,6,8],[1,3,5,7]]
condenseHyperverses :: DimensionsortProps a -> Hyperverse a -> Hyperverse a
condenseHyperverses dsProps (Universe elems) = Megaverse sortedUniverses
  where
    universes = map Universe $ splitEvery (versesize dsProps) elems
    subAlg = subAlgorithm dsProps
    sortedUniverses = map subAlg universes
condenseHyperverses dsProps (Megaverse verses) = Megaverse sortedVerses
  where
    megaverses = map Megaverse $ splitEvery (versesize dsProps) verses
    subAlg = subAlgorithm dsProps
    sortedVerses = map subAlg megaverses
