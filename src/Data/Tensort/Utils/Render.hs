module Data.Tensort.Utils.Render (getSortedBitsFromTensor) where

import Data.Maybe (isNothing)
import Data.Tensort.Utils.Compose (createTensor)
import Data.Tensort.Utils.Types (Bit, Memory (..), SortAlg, Sortable (..), Tensor, TensorStack, fromJust, fromSortBit, WonkyState)

-- | Compile a sorted list of Bits from a list of TensorStacks

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getSortedBitsFromTensor ([(0,5),(1,7)],ByteMem [[1,5],[3,7]]) bubblesort
-- [1,3,5,7]
-- >>> getSortedBitsFromTensor ([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])]) bubblesort
-- [1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18]
getSortedBitsFromTensor :: TensorStack -> SortAlg ->WonkyState -> ([Bit], WonkyState)
getSortedBitsFromTensor tensorRaw subAlg wonkySt = acc tensorRaw ([], wonkySt)
  where
    acc :: TensorStack -> ([Bit], WonkyState) -> ([Bit], WonkyState)
    acc tensor (sortedBits, wonkySt') = do
      let ((nextBit, tensor'), wonkySt'') = removeTopBitFromTensor tensor subAlg wonkySt'
      if isNothing tensor'
        then (nextBit : sortedBits, wonkySt'')
        else do
          acc (fromJust tensor') ((nextBit : sortedBits), wonkySt'')

-- | For use in compiling a list of Tensors into a sorted list of Bits
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> removeTopBitFromTensor  ([(0,5),(1,7)],ByteMem [[1,5],[3,7]]) bubblesort
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBitFromTensor :: Tensor -> SortAlg -> WonkyState -> ((Bit, Maybe Tensor), WonkyState)
removeTopBitFromTensor (register, memory) subAlg wonkySt = do
  let topRecord = last register
  let topAddress = fst topRecord
  let ((topBit, memory'), wonkySt') = removeBitFromMemory memory topAddress subAlg wonkySt
  if isNothing memory'
    then ((topBit, Nothing), wonkySt')
    else do
    let (tensor, wonkySt'') = createTensor (fromJust memory') subAlg wonkySt'
    ((topBit, Just (tensor)), wonkySt'')

removeBitFromMemory :: Memory -> Int -> SortAlg -> WonkyState -> ((Bit, Maybe Memory), WonkyState)
removeBitFromMemory (ByteMem bytes) i subAlg wonkySt = do
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
      let (result, wonkySt') = subAlg (SortBit topByte') wonkySt
      let topByte'' = fromSortBit result
      let bytes' = take i bytes ++ [topByte''] ++ drop (i + 1) bytes
      ((topBit, Just (ByteMem bytes')), wonkySt')
removeBitFromMemory (TensorMem tensors) i subAlg wonkySt = do
  let topTensor = tensors !! i
  let ((topBit, topTensor'), wonkySt') = removeTopBitFromTensor topTensor subAlg wonkySt
  if isNothing topTensor'
    then do
      let tensors' = take i tensors ++ drop (i + 1) tensors
      if null tensors'
        then ((topBit, Nothing), wonkySt')
        else ((topBit, Just (TensorMem tensors')), wonkySt')
    else do
      let tensors' = take i tensors ++ [fromJust topTensor'] ++ drop (i + 1) tensors
      ((topBit, Just (TensorMem tensors')), wonkySt')
