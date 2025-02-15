-- | Module for rendering a sorted list of Elements from a Megaverse.
module Data.Tensort.Dimensionsort.Render (getSortedElems) where

import Data.Tensort.Dimensionsort.Types
  ( Element,
    Hyperverse (..),
    SortAlg,
  )

-- | Compile a sorted list of Elements from a Megaverse.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Dimensionsort.Types (Hyperverse (..))
-- >>> getSortedElems bubblesort (Megaverse (Universe [5,7],Universe [1,3]))
-- [1,3,5,7]
-- >>> getSortedElems bubblesort (Megaverse [Megaverse [Universe [5,7],Universe [1,3]],Megaverse [Universe [2,4],Universe [6,8]]])
-- [1,2,3,4,5,6,7,8]
getSortedElems :: (Ord a) => SortAlg a -> Hyperverse a -> [Element a]
getSortedElems subAlg (Universe uni) = acc (Universe uni) []
  where
    acc (Universe []) sortedBits = sortedBits
    acc universe sortedBits = acc universe' (nextBit : sortedBits)
      where
        (nextBit, universe') = removeTopElem subAlg universe
getSortedElems subAlg (Megaverse mega) = acc (Megaverse mega) []
  where
    acc (Megaverse []) sortedBits = sortedBits
    acc megaverse sortedBits = acc megaverse' (nextBit : sortedBits)
      where
        (nextBit, megaverse') = removeTopElem subAlg megaverse

-- | For use in compiling a list of Tensors into a sorted list of Elements.
--
-- | Removes the top Element from a Hyperverse, rebalances the Hyperverse and returns
--   the removed Element along with the rebalanced Hyperverse.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Dimensionsort.Types (Hyperverse (..))
--   >>> removeTopElem bubblesort (Universe [5, 1, 7])
--   (7,Universe [1,5])
--
--   >>> removeTopElem bubblesort (Universe [5])
--   (5,Universe [])
--
--   >>> removeTopElem bubblesort (Universe [])
--   *** Exception: Cannot remove top bit from empty Hyperverse
--
--   >>> removeTopElem bubblesort (Megaverse [Universe [5, 7], Universe [1, 3]])
--   (3, Megaverse [Universe [1], Universe [5, 7]])
--
--   >>> removeTopElem bubblesort (Megaverse [Universe [5], Universe [7]])
--   (7,Megaverse [Universe [5]])
--
--   >>> removeTopElem bubblesort (Megaverse [Universe [5]])
--   (5,Megaverse [])
--
--   >>> removeTopElem bubblesort (Megaverse [])
--   *** Exception: Cannot remove top bit from empty Hyperverse
removeTopElem :: (Ord a) => SortAlg a -> Hyperverse a -> (Element a, Hyperverse a)
removeTopElem _ (Universe []) = error "Cannot remove top bit from empty Hyperverse"
removeTopElem _ (Universe [element]) = (element, Universe [])
removeTopElem subAlg (Universe elems) = (topElement, sortedElems)
  where
    topElement = last elems
    sortedElems = subAlg . Universe $ init elems
removeTopElem _ (Megaverse []) = error "Cannot remove top bit from empty Hyperverse"
removeTopElem subAlg (Megaverse [hyperverse]) = removeTopElem subAlg hyperverse
removeTopElem subAlg (Megaverse hyperverses) = (topElem, sortedMegaverse)
  where
    (topElem, topVerse') = removeTopElem subAlg topVerse
    topVerse = last hyperverses
    megaverse = Megaverse $ topVerse' : hyperverses
    sortedMegaverse = subAlg megaverse
