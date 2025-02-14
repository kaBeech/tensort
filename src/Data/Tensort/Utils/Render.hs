-- | Module for rendering a sorted list of Bits from a list of TensorStacks.
--
--   Functions ending in "B" are for sorting Bits in a base (non-recursive)
--   Tensort variant.
--
--   Functions ending in "R" are for sorting Records when used in a recursive
--   Tensort variant.
--
--   TODO: See if we can clean up the type conversion here.
module Data.Tensort.Utils.Render (getSortedBits) where

import Data.Maybe (isNothing)
import Data.Tensort.Utils.Compose (createTensor)
import Data.Tensort.Utils.Types
  ( Bit,
    Memory (..),
    Record,
    Register,
    SortAlg,
    Tensor,
    TensorStack,
    fromJust,
    fromRecord,
  )

-- | Compile a sorted list of Bits from a list of TensorStacks.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getSortedBits bubblesort (STensorBit ([(0,5),(1,7)],ByteMem [[1,5],[3,7]]))
-- [SBitBit 1,SBitBit 3,SBitBit 5,SBitBit 7]
-- >>> getSortedBits bubblesort (STensorBit ([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])]))
-- [SBitBit 1,SBitBit 2,SBitBit 3,SBitBit 4,SBitBit 5,SBitBit 6,SBitBit 7,SBitBit 8,SBitBit 11,SBitBit 12,SBitBit 13,SBitBit 14,SBitBit 15,SBitBit 16,SBitBit 17,SBitBit 18]
getSortedBits :: (Ord a) => SortAlg (Bit a) -> SortAlg (Record a) -> TensorStack a -> [Bit a]
getSortedBits subAlgB subAlgR tensorRaw = acc tensorRaw []
  where
    acc tensor sortedBits =
      if isNothing tensor'
        then nextBit' : sortedBits
        else acc (fromJust tensor') (nextBit' : sortedBits)
      where
        (nextBit, tensor') = removeTopBit subAlgB subAlgR tensor
        nextBit' = nextBit

-- | For use in compiling a list of Tensors into a sorted list of Bits.
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> removeTopBit bubblesort ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBit :: (Ord a) => SortAlg (Bit a) -> SortAlg (Record a) -> (Register a, Memory a) -> (Bit a, Maybe (Tensor a))
removeTopBit subAlgB subAlgR (register, memory) =
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, tensor)
  where
    (topBit, memory') = removeBit subAlgB subAlgR memory topAddress
    topRecord = last register
    topAddress = snd $ fromRecord topRecord
    tensor = Just tensorRaw
    tensorRaw = createTensor subAlgR memRefined
    memRefined = fromJust memory'

removeBit :: (Ord a) => SortAlg (Bit a) -> SortAlg (Record a) -> Memory a -> Int -> (Bit a, Maybe (Memory a))
removeBit subAlgB _ (ByteMem bytes) i =
  case length topByte' of
    0 ->
      let bytes' = left ++ right
       in if null bytes'
            then (topBit, Nothing)
            else (topBit, justMem bytes')
    1 ->
      let bytes' = left ++ [topByte'] ++ right
       in (topBit, justMem bytes')
    _ ->
      let bytes' = left ++ [topByte''] ++ right
          topByte'' = subAlgB topByte'
       in (topBit, justMem bytes')
  where
    topByte = bytes !! i
    topBit = last topByte
    topByte' = init topByte
    justMem = Just . ByteMem
    left = take i bytes
    right = drop (i + 1) bytes
removeBit subAlgB subAlgR (TensorMem tensors) i
  | isNothing topTensor' =
      let tensors' = left ++ right
       in if null tensors'
            then (topBit, Nothing)
            else (topBit, justMem tensors')
  | otherwise =
      let tensors' = left ++ [fromJust topTensor'] ++ right
       in (topBit, justMem tensors')
  where
    topTensor = tensors !! i
    (topBit, topTensor') = removeTopBit subAlgB subAlgR topTensor
    justMem = Just . TensorMem
    left = take i tensors
    right = drop (i + 1) tensors
