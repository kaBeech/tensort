module Data.Tensort.Utils.Render (getSortedBitsFromMetastack) where

import Data.Maybe (isNothing)
import Data.Tensort.Utils.Tensor (createTensor)
import Data.Tensort.Utils.Types (Memory (..), SortAlg, Sortable (..), Tensor, TensorStack, fromJust, fromSortBit)

-- | Compile a sorted list of Bits from a list of TensorStacks

-- | ==== __Examples__
--  >>> getSortedBitsFromMetastack ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--  [1,3,5,7]
--  >>> getSortedBitsFromMetastack ([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])])
--  [1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18]
getSortedBitsFromMetastack :: TensorStack -> SortAlg -> [Int]
getSortedBitsFromMetastack metastackRaw subAlg = acc metastackRaw []
  where
    acc :: TensorStack -> [Int] -> [Int]
    acc metastack sortedBits = do
      let (nextBit, metastack') = removeTopBitFromTensor metastack subAlg
      if isNothing metastack'
        then nextBit : sortedBits
        else do
          acc (fromJust metastack') (nextBit : sortedBits)

-- | For use in compiling a list of Tensors into a sorted list of Bits
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor

-- | ==== __Examples__
--   >>> removeTopBitFromTensor  ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBitFromTensor :: Tensor -> SortAlg -> (Int, Maybe Tensor)
removeTopBitFromTensor (register, memory) tsProps = do
  let topRecord = last register
  let topAddress = fst topRecord
  let (topBit, memory') = removeBitFromMemory memory topAddress tsProps
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, Just (createTensor (fromJust memory') tsProps))

removeBitFromMemory :: Memory -> Int -> SortAlg -> (Int, Maybe Memory)
removeBitFromMemory (ByteMem bytes) i subAlg = do
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
removeBitFromMemory (TensorMem tensors) i subAlg = do
  let topTensor = tensors !! i
  let (topBit, topTensor') = removeTopBitFromTensor topTensor subAlg
  if isNothing topTensor'
    then do
      let tensors' = take i tensors ++ drop (i + 1) tensors
      if null tensors'
        then (topBit, Nothing)
        else (topBit, Just (TensorMem tensors'))
    else do
      let tensors' = take i tensors ++ [fromJust topTensor'] ++ drop (i + 1) tensors
      (topBit, Just (TensorMem tensors'))
