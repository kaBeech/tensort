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
    acc tensor sortedBits = do
      let (nextBit, tensor') = removeTopBitB subAlg tensor
      let nextBit' = SBitBit nextBit
      if isNothing tensor'
        then nextBit' : sortedBits
        else do
          acc (fromJust tensor') (nextBit' : sortedBits)

getSortedBitsR :: SortAlg -> TensorStackR -> [SBit]
getSortedBitsR subAlg tensorRaw = acc tensorRaw []
  where
    acc :: TensorStackR -> [SBit] -> [SBit]
    acc tensor sortedBits = do
      let (nextBit, tensor') = removeTopBitR subAlg tensor
      let nextBit' = SBitRec nextBit
      if isNothing tensor'
        then nextBit' : sortedBits
        else do
          acc (fromJust tensor') (nextBit' : sortedBits)

-- | For use in compiling a list of Tensors into a sorted list of Bits.
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> removeTopBitB bubblesort ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBitB :: SortAlg -> Tensor -> (Bit, Maybe Tensor)
removeTopBitB subAlg (register, memory) = do
  let topRecord = last register
  let topAddress = fst topRecord
  let (topBit, memory') = removeBitB subAlg memory topAddress
  if isNothing memory'
    then (topBit, Nothing)
    else
      ( topBit,
        Just
          ( fromSTensorBit
              ( createTensor
                  subAlg
                  (SMemoryBit (fromJust memory'))
              )
          )
      )

removeTopBitR :: SortAlg -> TensorR -> (BitR, Maybe TensorR)
removeTopBitR subAlg (register, memory) = do
  let topRecord = last register
  let topAddress = fst topRecord
  let (topBit, memory') = removeBitR subAlg memory topAddress
  if isNothing memory'
    then (topBit, Nothing)
    else
      ( topBit,
        Just
          ( fromSTensorRec
              ( createTensor
                  subAlg
                  (SMemoryRec (fromJust memory'))
              )
          )
      )

removeBitB :: SortAlg -> Memory -> Int -> (Bit, Maybe Memory)
removeBitB subAlg (ByteMem bytes) i = do
  let topByte = bytes !! i
  let topBit = last topByte
  let topByte' = init topByte
  case length topByte' of
    0 -> do
      let bytes' = take i bytes ++ drop (i + 1) bytes
      if null bytes'
        then (topBit, Nothing)
        else (topBit, Just (ByteMem bytes'))
    1 -> do
      let bytes' = take i bytes ++ [topByte'] ++ drop (i + 1) bytes
      (topBit, Just (ByteMem bytes'))
    _ -> do
      let topByte'' = fromSortBit (subAlg (SortBit topByte'))
      let bytes' = take i bytes ++ [topByte''] ++ drop (i + 1) bytes
      (topBit, Just (ByteMem bytes'))
removeBitB subAlg (TensorMem tensors) i = do
  let topTensor = tensors !! i
  let (topBit, topTensor') = removeTopBitB subAlg topTensor
  if isNothing topTensor'
    then do
      let tensors' = take i tensors ++ drop (i + 1) tensors
      if null tensors'
        then (topBit, Nothing)
        else (topBit, Just (TensorMem tensors'))
    else do
      let tensors' =
            take i tensors ++ [fromJust topTensor'] ++ drop (i + 1) tensors
      (topBit, Just (TensorMem tensors'))

removeBitR :: SortAlg -> MemoryR -> Int -> (BitR, Maybe MemoryR)
removeBitR subAlg (ByteMemR bytesR) i = do
  let topByteR = bytesR !! i
  let topBitR = last topByteR
  let topByteR' = init topByteR
  case length topByteR' of
    0 -> do
      let bytesR' = take i bytesR ++ drop (i + 1) bytesR
      if null bytesR'
        then (topBitR, Nothing)
        else (topBitR, Just (ByteMemR bytesR'))
    1 -> do
      let bytesR' = take i bytesR ++ [topByteR'] ++ drop (i + 1) bytesR
      (topBitR, Just (ByteMemR bytesR'))
    _ -> do
      let topByteR'' = fromSortRec (subAlg (SortRec topByteR'))
      let bytesR' = take i bytesR ++ [topByteR''] ++ drop (i + 1) bytesR
      (topBitR, Just (ByteMemR bytesR'))
removeBitR subAlg (TensorMemR tensorsR) i = do
  let topTensorR = tensorsR !! i
  let (topBitR, topTensorR') = removeTopBitR subAlg topTensorR
  if isNothing topTensorR'
    then do
      let tensorsR' = take i tensorsR ++ drop (i + 1) tensorsR
      if null tensorsR'
        then (topBitR, Nothing)
        else (topBitR, Just (TensorMemR tensorsR'))
    else do
      let tensorsR' =
            take i tensorsR ++ [fromJust topTensorR'] ++ drop (i + 1) tensorsR
      (topBitR, Just (TensorMemR tensorsR'))
