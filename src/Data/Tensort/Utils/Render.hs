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
    BitR,
    Memory (..),
    MemoryR (..),
    SBit (..),
    SMemory (..),
    STensor (..),
    STensorStack,
    SortAlg,
    Sortable (..),
    Tensor,
    TensorR,
    TensorStack,
    TensorStackR,
    fromJust,
    fromSTensorBit,
    fromSTensorRec,
    fromSortBit,
    fromSortRec,
  )

-- | Compile a sorted list of Bits from a list of TensorStacks.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getSortedBits bubblesort (STensorBit ([(0,5),(1,7)],ByteMem [[1,5],[3,7]]))
-- [SBitBit 1,SBitBit 3,SBitBit 5,SBitBit 7]
-- >>> getSortedBits bubblesort (STensorBit ([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])]))
-- [SBitBit 1,SBitBit 2,SBitBit 3,SBitBit 4,SBitBit 5,SBitBit 6,SBitBit 7,SBitBit 8,SBitBit 11,SBitBit 12,SBitBit 13,SBitBit 14,SBitBit 15,SBitBit 16,SBitBit 17,SBitBit 18]
getSortedBits :: SortAlg -> STensorStack -> [SBit]
getSortedBits subAlg (STensorBit tensorRaw) =
  getSortedBitsB subAlg tensorRaw
getSortedBits subAlg (STensorRec tensorRaw) =
  getSortedBitsR subAlg tensorRaw

getSortedBitsB :: SortAlg -> TensorStack -> [SBit]
getSortedBitsB subAlg tensorRaw = acc tensorRaw []
  where
    acc :: TensorStack -> [SBit] -> [SBit]
    acc tensor sortedBits =
      if isNothing tensor'
        then nextBit' : sortedBits
        else acc (fromJust tensor') (nextBit' : sortedBits)
      where
        (nextBit, tensor') = removeTopBitB subAlg tensor
        nextBit' = SBitBit nextBit

getSortedBitsR :: SortAlg -> TensorStackR -> [SBit]
getSortedBitsR subAlg tensorRaw = acc tensorRaw []
  where
    acc :: TensorStackR -> [SBit] -> [SBit]
    acc tensor sortedBits =
      if isNothing tensor'
        then nextBit' : sortedBits
        else acc (fromJust tensor') (nextBit' : sortedBits)
      where
        (nextBit, tensor') = removeTopBitR subAlg tensor
        nextBit' = SBitRec nextBit

-- | For use in compiling a list of Tensors into a sorted list of Bits.
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> removeTopBitB bubblesort ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBitB :: SortAlg -> Tensor -> (Bit, Maybe Tensor)
removeTopBitB subAlg (register, memory) =
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, tensor)
  where
    (topBit, memory') = removeBitB subAlg memory topAddress
    topRecord = last register
    topAddress = fst topRecord
    tensor = Just $ fromSTensorBit tensorRaw
    tensorRaw = createTensor subAlg memRefined
    memRefined = SMemoryBit $ fromJust memory'

removeTopBitR :: SortAlg -> TensorR -> (BitR, Maybe TensorR)
removeTopBitR subAlg (register, memory) =
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, tensorR)
  where
    (topBit, memory') = removeBitR subAlg memory topAddress
    topRecord = last register
    topAddress = fst topRecord
    tensorR = Just $ fromSTensorRec tensorRawR
    tensorRawR = createTensor subAlg memRefinedR
    memRefinedR = SMemoryRec (fromJust memory')

removeBitB :: SortAlg -> Memory -> Int -> (Bit, Maybe Memory)
removeBitB subAlg (ByteMem bytes) i =
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
          topByte'' = fromSortBit . subAlg $ SortBit topByte'
       in (topBit, justMem bytes')
  where
    topByte = bytes !! i
    topBit = last topByte
    topByte' = init topByte
    justMem = Just . ByteMem
    left = take i bytes
    right = drop (i + 1) bytes
removeBitB subAlg (TensorMem tensors) i
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
    (topBit, topTensor') = removeTopBitB subAlg topTensor
    justMem = Just . TensorMem
    left = take i tensors
    right = drop (i + 1) tensors

removeBitR :: SortAlg -> MemoryR -> Int -> (BitR, Maybe MemoryR)
removeBitR subAlg (ByteMemR bytesR) i =
  let topByteR = bytesR !! i
      topBitR = last topByteR
      topByteR' = init topByteR
      justMem = Just . ByteMemR
      left = take i bytesR
      right = drop (i + 1) bytesR
   in case length topByteR' of
        0 ->
          if null bytesR'
            then (topBitR, Nothing)
            else (topBitR, justMem bytesR')
          where
            bytesR' = left ++ right
        1 ->
          (topBitR, justMem bytesR')
          where
            bytesR' = left ++ [topByteR'] ++ right
        _ ->
          (topBitR, justMem bytesR')
          where
            topByteR'' = fromSortRec (subAlg (SortRec topByteR'))
            bytesR' = left ++ [topByteR''] ++ right
removeBitR subAlg (TensorMemR tensorsR) i
  | isNothing topTensorR' =
      let tensorsR' = left ++ right
       in if null tensorsR'
            then (topBitR, Nothing)
            else (topBitR, justMem tensorsR')
  | otherwise =
      let tensorsR' = left ++ [fromJust topTensorR'] ++ right
       in (topBitR, justMem tensorsR')
  where
    topTensorR = tensorsR !! i
    (topBitR, topTensorR') = removeTopBitR subAlg topTensorR
    justMem = Just . TensorMemR
    left = take i tensorsR
    right = drop (i + 1) tensorsR
