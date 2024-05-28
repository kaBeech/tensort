-- Note: The Bytes utilities are split into two modules because my LSP was having
-- difficulty keeping up when it was all in one file. For the official release I
-- plan to either condense into one file or modularize into multiple files
module Data.Robustsort.Utils.Bytes2
  ( getSortedBitsFromBytestacks,
  )
where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesortRecords)
import Data.Robustsort.Utils.Bytes (createBytestore)
import Data.Robustsort.Utils.Types (Bytestack, Bytestore, Memory (..))

-- | Compile a sorted list of Bits from a list of Bytestacks

-- | ==== __Examples__
-- >>> getSortedBitsFromBytestacks [([(0,28),(1,38)],BigMemory [([(0,27),(1,28)],BigMemory [([(0,23),(1,27)],SmallMemory [[21,23],[25,27]]),([(0,24),(1,28)],SmallMemory [[22,24],[26,28]])]),([(1,37),(0,38)],BigMemory [([(0,33),(1,38)],SmallMemory [[31,33],[35,38]]),([(0,34),(1,37)],SmallMemory [[32,14],[36,37]])])])]
-- [14, 21, 22, 23, 24, 25, 26, 27, 28, 31, 32, 33, 35, 36, 37, 38]
getSortedBitsFromBytestacks :: [Bytestack] -> [Int]
getSortedBitsFromBytestacks bytestacksRaw = acc bytestacksRaw []
  where
    acc :: [Bytestack] -> [Int] -> [Int]
    acc bytestacks sortedBits =
      if areAllEmpty bytestacks
        then sortedBits
        else acc bytestacks' (nextBit : sortedBits)
      where
        (nextBit, bytestacks') = removeNextBitFromBytestacks bytestacks

-- | Returns True if registers for all Bytestacks are empty and False otherwise

-- | ==== __Examples__
-- >>> areAllEmpty [([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])]
-- False
-- >>> areAllEmpty [([],SmallMemory [[1,3],[5,7]]),([],SmallMemory [[2,4],[6,8]])]
-- True
areAllEmpty :: [Bytestack] -> Bool
areAllEmpty = all isEmpty
  where
    isEmpty :: Bytestack -> Bool
    isEmpty bytestack = null (fst bytestack)

-- | For use in compiling a list of Bytestacks into a sorted list of Bits

-- | Removes the top Bit from the top Bytestack, rebalances the Bytestacks, and
--   returns the removed Bit and the rebalanced Bytestacks

-- | ==== __Examples__
-- >>> removeNextBitFromBytestacks [([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(1,4), (0,8)],SmallMemory [[6,8],[2,4]])]
-- (8,[([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(1,4),(0,6)],SmallMemory [[2,4],[6]])])
removeNextBitFromBytestacks :: [Bytestack] -> (Int, [Bytestack])
removeNextBitFromBytestacks bytestacks = do
  let topRecords = bubblesortRecords (getTopRecordsFromBytestacks bytestacks)
  let topBytestackIndex = fst (last topRecords)
  let topBytestack = bytestacks !! topBytestackIndex
  let (nextBit, bytestack') = removeTopBitFromBytestore topBytestack
  let newBytestacks = take topBytestackIndex bytestacks ++ [bytestack'] ++ drop (topBytestackIndex + 1) bytestacks
  (nextBit, newBytestacks)

getTopRecordsFromBytestacks :: [Bytestack] -> [(Int, Int)]
getTopRecordsFromBytestacks = map (last . fst)

-- | For use in compiling a list of Bytestores into a sorted list of Bits
--
-- | Removes the top Bit from a Bytestore, rebalances the Bytestore and returns
--   the removed Bit along with the rebalanced Bytestore

-- | ==== __Examples__
--   >>> removeTopBitFromBytestore  ([(0,5),(1,7)],SmallMemory [[1,5],[3,7]])
--   (7,([(1,3),(0,5)],SmallMemory [[1,5],[3]]))
removeTopBitFromBytestore :: Bytestore -> (Int, Bytestore)
removeTopBitFromBytestore (register, memory) = do
  let topRecord = last register
  let topAddress = fst topRecord
  let (topBit, memory') = removeBitFromMemory memory topAddress
  (topBit, createBytestore memory')

removeBitFromMemory :: Memory -> Int -> (Int, Memory)
removeBitFromMemory (SmallMemory bytes) i = do
  let topByte = bytes !! i
  let topBit = last topByte
  let topByte' = init topByte
  let bytes' = take i bytes ++ [topByte'] ++ drop (i + 1) bytes
  (topBit, SmallMemory bytes')
removeBitFromMemory (BigMemory bytestores) i = do
  let topBytestore = bytestores !! i
  let (topBit, topBytestore') = removeTopBitFromBytestore topBytestore
  let bytestores' = take i bytestores ++ [topBytestore'] ++ drop (i + 1) bytestores
  (topBit, BigMemory bytestores')
