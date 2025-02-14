-- | Module for rendering a sorted list of Bits from a list of TensorStacks.
module Data.Tensort.Utils.Render (getSortedBits) where

import Data.Maybe (isNothing)
import Data.Tensort.Utils.Compose (createTensor)
import Data.Tensort.Utils.Types
  ( Bit,
    Memory (..),
    Register,
    SortAlg,
    Tensor (..),
    TopBit,
    fromJust,
    fromRecord,
    fromTensor,
  )

-- | Compile a sorted list of Bits from a list of TensorStacks.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.Types (Record (..))
-- >>> getSortedBits bubblesort (Tensor ([Record (0,5),Record (1,7)],ByteMem [[1,5],[3,7]]))
-- [1,3,5,7]
-- >>> getSortedBits bubblesort (Tensor ([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])]))
-- [1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18]
getSortedBits :: (Ord a) => SortAlg a -> Tensor a -> [Bit a]
getSortedBits subAlg tensorRaw = acc (fromTensor tensorRaw) []
  where
    acc tensor sortedBits =
      if isNothing tensor'
        then nextBit' : sortedBits
        else acc (fromJust tensor') (nextBit' : sortedBits)
      where
        (nextBit, tensor') = removeTopBit subAlg tensor
        nextBit' = nextBit

-- | For use in compiling a list of Tensors into a sorted list of Bits.
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Utils.Types (Record (..))
--   >>> removeTopBit bubblesort ([Record (0,5),Record (1,7)],ByteMem [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBit :: (Ord a) => SortAlg a -> (Register a, Memory a) -> (TopBit a, Maybe (Register a, Memory a))
removeTopBit subAlg (register, memory) =
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, tensor)
  where
    (topBit, memory') = removeBit subAlg memory topAddress
    topRecord = last register
    topAddress = snd $ fromRecord topRecord
    tensor = Just tensorRaw
    tensorRaw = fromTensor $ createTensor subAlg memRefined
    memRefined = fromJust memory'

removeBit :: (Ord a) => SortAlg a -> Memory a -> Int -> (TopBit a, Maybe (Memory a))
removeBit subAlg (ByteMem bytes) i =
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
          topByte'' = subAlg topByte'
       in (topBit, justMem bytes')
  where
    topByte = bytes !! i
    topBit = last topByte
    topByte' = init topByte
    justMem = Just . ByteMem
    left = take i bytes
    right = drop (i + 1) bytes
removeBit subAlg (TensorMem tensors) i
  | isNothing topTensor' =
      let tensors' = left ++ right
       in if null tensors'
            then (topBit, Nothing)
            else (topBit, justMem tensors')
  | otherwise =
      let tensors' = left ++ [Tensor (fromJust topTensor')] ++ right
       in (topBit, justMem tensors')
  where
    topTensor = tensors !! i
    (topBit, topTensor') = removeTopBit subAlg $ fromTensor topTensor
    justMem = Just . TensorMem
    left = take i tensors
    right = drop (i + 1) tensors
