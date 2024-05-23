module Data.Robustsort.Utils.Bytes2
  ( getSortedListFromBytestacks,
  )
where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesortRecords)
import Data.Robustsort.Utils.Types (Bytestack, Memory (..))

-- | Compile a sorted list of Bits from a list of Bytestacks

-- | ==== __Examples__
-- >>> getSortedListFromBytestacks [([(0,28),(1,38)],BigMemory [([(0,27),(1,28)],BigMemory [([(0,23),(1,27)],Memory [[21,23],[25,27]]),([(0,24),(1,28)],Memory [[22,24],[26,28]])]),([(1,37),(0,38)],BigMemory [([(0,33),(1,38)],Memory [[31,33],[35,38]]),([(0,34),(1,37)],Memory [[32,14],[36,37]])])])]
-- [14, 21, 22, 23, 24, 25, 26, 27, 28, 31, 32, 33, 35, 36, 37, 38]
getSortedListFromBytestacks :: [Bytestack] -> [Int]
getSortedListFromBytestacks bytestacksRaw = acc bytestacksRaw []
  where
    acc :: [Bytestack] -> [Int] -> [Int]
    acc bytestacks sortedBits =
      if areAllEmpty bytestacks
        then sortedBits
        else acc bytestacks' (nextBit : sortedBits)
      where
        (nextBit, bytestacks') = getNextBitFromBytestacks bytestacks

-- | Returns True if registers for all Bytestacks are empty and False otherwise

-- | ==== __Examples__
-- >>> areAllEmpty [([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(0,4),(1,8)],Memory [[2,4],[6,8]])]
-- False
-- >>> areAllEmpty [([],Memory [[1,3],[5,7]]),([],Memory [[2,4],[6,8]])]
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
-- >>> getNextBitFromBytestacks [([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(1,4), (0,8)],Memory [[6,8],[2,4]])]
-- (8,[([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(1,4),(0,6)],Memory [[2,4],[6]])])
getNextBitFromBytestacks :: [Bytestack] -> (Int, [Bytestack])
getNextBitFromBytestacks bytestacks = do
  let topRecords = bubblesortRecords (getTopRecordsFromBytestacks bytestacks)
  let topBytestackIndex = fst (last topRecords)
  let topBytestack = bytestacks !! topBytestackIndex
  let (nextBit, bytestack') = removeTopValueFromBytestack topBytestack
  let newBytestacks = take topBytestackIndex bytestacks ++ [bytestack'] ++ drop (topBytestackIndex + 1) bytestacks
  (nextBit, newBytestacks)

getTopRecordsFromBytestacks :: [Bytestack] -> [(Int, Int)]
getTopRecordsFromBytestacks = map (last . fst)

-- This is a dummy function to be edited later
removeTopValueFromBytestack :: Bytestack -> (Int, Bytestack)
removeTopValueFromBytestack (register, memory) = (topBit, (init register, memory))
  where
    topBit = snd (last register)
