-- | This module provides variations of the Dimensionsort algorithm
module Data.Tensort.Dimensionsort
  ( dimensionsort,
    dimensionsortB4,
    dimensionortBN,
    dimensionsortBL,
  )
where

-- import Data.Tensort.Dimensionsort.Compose (createInitialMegaverses)
import Data.Tensort.Dimensionsort.Convert (rawElemsToUniverses)
import Data.Tensort.Dimensionsort.MkDsProps (mkDsProps)
import Data.Tensort.Dimensionsort.Types
  ( DimensionsortProps (..),
    Element,
    Hyperverse (..),
  )
import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Utils.LogNat (getLnLength)
import Data.Tensort.Utils.RandomizeList (randomizeList)

-- import Data.Tensort.Dimensionsort.Reduce (combineMegaverses)
-- import Data.Tensort.Dimensionsort.Render (getSortedElems)

-- | Sort a list using a custom Dimensionsort algorithm
--
--   Takes DimensionsortProps (Versesize and SubAlgorithm) and a list and returns
--   a sorted list

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Dimensionsort.MkDsProps (mkDsProps)
-- >>> dimensionsort (mkDsProps 2 bubblesort) ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> dimensionsort (mkDsProps 2 bubblesort) ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
dimensionsort :: (Ord a) => DimensionsortProps a -> Hyperverse a -> Hyperverse a
dimensionsort _ (Universe []) = Universe []
dimensionsort _ (Universe [x]) = Universe [x]
dimensionsort tsProps (Universe [x, y]) = subAlgorithm tsProps uni
  where
    uni = Universe [x, y]
dimensionsort tsProps xs = getSortedElems subAlg topMegaverse
  where
    subAlg = subAlgorithm tsProps
    topMegaverse = combineMegaverses tsProps megaverses
    megaverses = createInitialMegaverses tsProps universes
    universes = rawElemsToUniverses tsProps elems
    elems = randomizeList 143 xs

-- | Sort a list using a Standard Dimensionsort algorithm with a 4-Element
--   Versesize

-- | ==== __Examples__
-- >>> dimensionsortB4 ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> dimensionsortB4 ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
dimensionsortB4 :: (Ord a) => [Element a] -> [Element a]
dimensionsortB4 = dimensionsort $ mkDsProps 4 bubblesort

-- | Sort a list using a Standard Dimensionsort algorithm with a custom
--   Versesize

-- | ==== __Examples__
-- >>> dimensionortBN 3 ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> dimensionortBN 3 ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
dimensionortBN :: (Ord a) => Int -> [Element a] -> [Element a]
dimensionortBN n = dimensionsort $ mkDsProps n bubblesort

-- | Sort a list using a Standard Logarithmic Dimensionsort algorithm
--
--   Standard Logarithmic Dimensionsort uses a Versesize that approximates the
--   natural logarithm of the length of the input list and a Bubblesort subalgorithm

-- | ==== __Examples__
-- >>> dimensionsortBL ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> dimensionsortBL ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
dimensionsortBL :: (Ord a) => [Element a] -> [Element a]
dimensionsortBL xs = dimensionsort tsProps xs
  where
    tsProps = mkDsProps (getLnLength xs) bubblesort
