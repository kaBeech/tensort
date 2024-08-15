module Data.Tensort.Utils.Render (getSortedBitsFromTensor) where

import Data.Maybe (isNothing)
import Data.Tensort.Utils.Compose (createTensor)
import Data.Tensort.Utils.Types (Bit, BitR, Memory (..), MemoryR (..), SBit (..), SMemory (..), STensor (..), STensorStack, SortAlg, Sortable (..), Tensor, TensorR, TensorStack, TensorStackR, WonkyState, fromJust, fromSTensorBit, fromSTensorRec, fromSortBit, fromSortRec)

-- | Compile a sorted list of Bits from a list of TensorStacks

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getSortedBitsFromTensor bubblesort (STensorBit ([(0,5),(1,7)],ByteMem [[1,5],[3,7]]))
-- [SBitBit 1,SBitBit 3,SBitBit 5,SBitBit 7]
-- >>> getSortedBitsFromTensor bubblesort (STensorBit ([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])]))
-- [SBitBit 1,SBitBit 2,SBitBit 3,SBitBit 4,SBitBit 5,SBitBit 6,SBitBit 7,SBitBit 8,SBitBit 11,SBitBit 12,SBitBit 13,SBitBit 14,SBitBit 15,SBitBit 16,SBitBit 17,SBitBit 18]
getSortedBitsFromTensor :: SortAlg -> WonkyState -> STensorStack -> ([SBit], WonkyState)
getSortedBitsFromTensor subAlg wonkySt (STensorBit tensorRaw) = getSortedBitsFromTensorB subAlg wonkySt tensorRaw
getSortedBitsFromTensor subAlg wonkySt (STensorRec tensorRaw) = getSortedBitsFromTensorR subAlg wonkySt tensorRaw

getSortedBitsFromTensorB :: SortAlg -> WonkyState -> TensorStack -> ([SBit], WonkyState)
getSortedBitsFromTensorB subAlg wonkySt tensorRaw = acc tensorRaw ([], wonkySt)
  where
    acc :: TensorStack -> ([SBit], WonkyState) -> ([SBit], WonkyState)
    acc tensor (sortedBits, wonkySt') = do
      let ((nextBit, tensor'), wonkySt'') = removeTopBitFromTensor subAlg wonkySt' tensor
      let nextBit' = SBitBit nextBit
      if isNothing tensor'
        then (nextBit' : sortedBits, wonkySt'')
        else acc (fromJust tensor') (nextBit' : sortedBits, wonkySt'')

getSortedBitsFromTensorR :: SortAlg -> WonkyState -> TensorStackR -> ([SBit], WonkyState)
getSortedBitsFromTensorR subAlg wonkySt tensorRaw = acc tensorRaw ([], wonkySt)
  where
    acc :: TensorStackR -> ([SBit], WonkyState) -> ([SBit], WonkyState)
    acc tensor (sortedBits, wonkySt') = do
      let ((nextBit, tensor'), wonkySt'') = removeTopBitFromTensorR subAlg wonkySt' tensor
      let nextBit' = SBitRec nextBit
      if isNothing tensor'
        then (nextBit' : sortedBits, wonkySt'')
        else acc (fromJust tensor') (nextBit' : sortedBits, wonkySt'')

-- | For use in compiling a list of Tensors into a sorted list of Bits
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> removeTopBitFromTensor bubblesort ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBitFromTensor :: SortAlg -> WonkyState -> Tensor -> ((Bit, Maybe Tensor), WonkyState)
removeTopBitFromTensor subAlg wonkySt (register, memory) = do
  let topRecord = last register
  let topAddress = fst topRecord
  let ((topBit, memory'), wonkySt') = removeBitFromMemory subAlg wonkySt memory topAddress
  if isNothing memory'
    then ((topBit, Nothing), wonkySt')
    else do
      let (tensor, wonkySt'') = createTensor subAlg wonkySt' (SMemoryBit (fromJust memory'))
      ((topBit, Just (fromSTensorBit tensor)), wonkySt'')

removeTopBitFromTensorR :: SortAlg -> WonkyState -> TensorR -> ((BitR, Maybe TensorR), WonkyState)
removeTopBitFromTensorR subAlg wonkySt (register, memory) = do
  let topRecord = last register
  let topAddress = fst topRecord
  let ((topBit, memory'), wonkySt') = removeBitFromMemoryR subAlg wonkySt memory topAddress
  if isNothing memory'
    then ((topBit, Nothing), wonkySt')
    else do
      let (tensor, wonkySt'') = createTensor subAlg wonkySt' (SMemoryRec (fromJust memory'))
      ((topBit, Just (fromSTensorRec tensor)), wonkySt'')

removeBitFromMemory :: SortAlg -> WonkyState -> Memory -> Int -> ((Bit, Maybe Memory), WonkyState)
removeBitFromMemory subAlg wonkySt (ByteMem bytes) i = do
  let topByte = bytes !! i
  let topBit = last topByte
  let topByte' = init topByte
  case length topByte' of
    0 -> do
      let bytes' = take i bytes ++ drop (i + 1) bytes
      if null bytes'
        then ((topBit, Nothing), wonkySt)
        else ((topBit, Just (ByteMem bytes')), wonkySt)
    1 -> do
      let bytes' = take i bytes ++ [topByte'] ++ drop (i + 1) bytes
      ((topBit, Just (ByteMem bytes')), wonkySt)
    _ -> do
      let (result, wonkySt') = subAlg wonkySt (SortBit topByte')
      let topByte'' = fromSortBit result
      let bytes' = take i bytes ++ [topByte''] ++ drop (i + 1) bytes
      ((topBit, Just (ByteMem bytes')), wonkySt')
removeBitFromMemory subAlg wonkySt (TensorMem tensors) i = do
  let topTensor = tensors !! i
  let ((topBit, topTensor'), wonkySt') = removeTopBitFromTensor subAlg wonkySt topTensor
  if isNothing topTensor'
    then do
      let tensors' = take i tensors ++ drop (i + 1) tensors
      if null tensors'
        then ((topBit, Nothing), wonkySt')
        else ((topBit, Just (TensorMem tensors')), wonkySt')
    else do
      let tensors' = take i tensors ++ [fromJust topTensor'] ++ drop (i + 1) tensors
      ((topBit, Just (TensorMem tensors')), wonkySt')

removeBitFromMemoryR :: SortAlg -> WonkyState -> MemoryR -> Int -> ((BitR, Maybe MemoryR), WonkyState)
removeBitFromMemoryR subAlg wonkySt (ByteMemR bytesR) i = do
  let topByteR = bytesR !! i
  let topBitR = last topByteR
  let topByteR' = init topByteR
  case length topByteR' of
    0 -> do
      let bytesR' = take i bytesR ++ drop (i + 1) bytesR
      if null bytesR'
        then ((topBitR, Nothing), wonkySt)
        else ((topBitR, Just (ByteMemR bytesR')), wonkySt)
    1 -> do
      let bytesR' = take i bytesR ++ [topByteR'] ++ drop (i + 1) bytesR
      ((topBitR, Just (ByteMemR bytesR')), wonkySt)
    _ -> do
      let (topByteR'', wonkySt') = subAlg wonkySt (SortRec topByteR')
      let bytesR' = take i bytesR ++ [fromSortRec topByteR''] ++ drop (i + 1) bytesR
      ((topBitR, Just (ByteMemR bytesR')), wonkySt')
removeBitFromMemoryR subAlg wonkySt (TensorMemR tensorsR) i = do
  let topTensorR = tensorsR !! i
  let ((topBitR, topTensorR'), wonkySt') = removeTopBitFromTensorR subAlg wonkySt topTensorR
  if isNothing topTensorR'
    then do
      let tensorsR' = take i tensorsR ++ drop (i + 1) tensorsR
      if null tensorsR'
        then ((topBitR, Nothing), wonkySt')
        else ((topBitR, Just (TensorMemR tensorsR')), wonkySt')
    else do
      let tensorsR' = take i tensorsR ++ [fromJust topTensorR'] ++ drop (i + 1) tensorsR
      ((topBitR, Just (TensorMemR tensorsR')), wonkySt')
