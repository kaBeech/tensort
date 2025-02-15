{-# LANGUAGE GADTs #-}

-- | This module provides types used in the Dimensionsort package.
module Data.Tensort.Dimensionsort.Types where

-- | DimensionsortProps contains the Versesize and SubAlgorithm used in a
--   Dimensionsort algorithm.
data DimensionsortProps a = DimensionsortProps
  { versesize :: Int,
    subAlgorithm :: SortAlg a
  }

-- | A Element is a single element of the list to be sorted.
type Element a = a

-- | A Universe is a list of Elements standardized to a fixed maximum length
--   (Versesize).

--   The length should be set either in or upstream of any function that uses
--   Elements.

-- | A Hyperverse is a recursive datatype containing the data to be sorted,
--   as a list of either Elements or smaller Hyperverses.
data Hyperverse a
  = Universe [Element a]
  | Megaverse [Hyperverse a]
  deriving (Show, Eq, Ord)

fromMegaverse :: Hyperverse a -> [Hyperverse a]
fromMegaverse (Megaverse m) = m
fromMegaverse _ = error "fromMegaverse: not a Megaverse"

fromUniverse :: Hyperverse a -> [Element a]
fromUniverse (Universe u) = u
fromUniverse _ = error "fromUniverse: not a Universe"

-- | A TopElement contains a copy of the last (i.e. highest) Element in a
--   Hyperverse
type TopElement a = Element a

-- | A sorting algorithm is a function that takes a list of ordered elements
--   and returns that list sorted.
type SortAlg a = Hyperverse a -> Hyperverse a

-- | Converts a regular SortAlg for use in Dimensionsort
sortAlgToDimensional :: (Ord a) => ([a] -> [a]) -> (Hyperverse a -> Hyperverse a)
sortAlgToDimensional alg = Universe . alg . fromUniverse

-- | Converts a Dimensionsort SortAlg to a regular SortAlg
sortAlgFromDimensional :: (Ord a) => (Hyperverse a -> Hyperverse a) -> ([a] -> [a])
sortAlgFromDimensional alg = fromUniverse . alg . Universe
