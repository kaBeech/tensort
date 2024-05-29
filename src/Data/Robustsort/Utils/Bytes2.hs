-- Note: The Bytes utilities are split into two modules because my LSP was having
-- difficulty keeping up when it was all in one file. For the official release I
-- plan to either condense into one file or modularize into multiple files
module Data.Robustsort.Utils.Bytes2
  ( getSortedBitsFromMetastack,
  )
where

import Data.Maybe (isNothing)
import Data.Robustsort.Utils.Bytes (createBytestore)
import Data.Robustsort.Utils.Types (Bytestack, Bytestore, Memory (..))

-- | Compile a sorted list of Bits from a list of Bytestacks

-- | ==== __Examples__
--  >>> getSortedBitsFromMetastack ([(0,5),(1,7)],SmallMemory [[1,5],[3,7]])
--  [1,3,5,7]
--  >>> getSortedBitsFromMetastack ([(0,8),(1,18)],BigMemory [([(0,7),(1,8)],BigMemory [([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])]),([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(0,14),(1,17)],SmallMemory [[12,14],[16,17]])])])
--  [1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18]
getSortedBitsFromMetastack :: Bytestack -> [Int]
getSortedBitsFromMetastack metastackRaw = acc metastackRaw []
  where
    acc :: Bytestack -> [Int] -> [Int]
    acc metastack sortedBits = do
      let (nextBit, metastack') = removeTopBitFromBytestore metastack
      if isNothing metastack'
        then nextBit : sortedBits
        else do
          acc (fromJust metastack') (nextBit : sortedBits)

-- | For use in compiling a list of Bytestores into a sorted list of Bits
--
-- | Removes the top Bit from a Bytestore, rebalances the Bytestore and returns
--   the removed Bit along with the rebalanced Bytestore

-- | ==== __Examples__
--   >>> removeTopBitFromBytestore  ([(0,5),(1,7)],SmallMemory [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],SmallMemory [[1,5],[3]]))
removeTopBitFromBytestore :: Bytestore -> (Int, Maybe Bytestore)
removeTopBitFromBytestore (register, memory) = do
  let topRecord = last register
  let topAddress = fst topRecord
  let (topBit, memory') = removeBitFromMemory memory topAddress
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, Just (createBytestore (fromJust memory')))

-- | ==== __Examples__
removeBitFromMemory :: Memory -> Int -> (Int, Maybe Memory)
removeBitFromMemory (SmallMemory bytes) i = do
  let topByte = bytes !! i
  let topBit = last topByte
  let topByte' = init topByte
  if null topByte'
    then do
      let bytes' = take i bytes ++ drop (i + 1) bytes
      if null bytes'
        then (topBit, Nothing)
        else (topBit, Just (SmallMemory bytes'))
    else do
      let bytes' = take i bytes ++ [topByte'] ++ drop (i + 1) bytes
      (topBit, Just (SmallMemory bytes'))
removeBitFromMemory (BigMemory bytestores) i = do
  let topBytestore = bytestores !! i
  let (topBit, topBytestore') = removeTopBitFromBytestore topBytestore
  if isNothing topBytestore'
    then do
      let bytestores' = take i bytestores ++ drop (i + 1) bytestores
      if null bytestores'
        then (topBit, Nothing)
        else (topBit, Just (BigMemory bytestores'))
    else do
      let bytestores' = take i bytestores ++ [fromJust topBytestore'] ++ drop (i + 1) bytestores
      (topBit, Just (BigMemory bytestores'))

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
