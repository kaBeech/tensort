-- module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestacksFromBytes, getSortedArrayFromBytestacks, reduceBytestacks) where
module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestoreFromBytes, getBytestacksFromBytes) where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Utils.Split (splitEvery)
import Data.Robustsort.Utils.Types (Byte, Bytestack, Bytestore, Memory (..), Metabyte (..), Record)

-- export default (bits: number[], bytesize: number): number[] => {
--     bits = randomizeArray(bits)
--     const bytes = convertRawBitsToBytes(bits, bytesize)
--     let bytestacks = getBytestacksFromBytes(bytes, bytesize)
--     while (bytestacks.length > bytesize) {
--         bytestacks = reduceBytestacks(bytestacks, bytesize)
--     }
--     return getSortedArrayFromBytestacks(bytestacks)
-- }

-- | Convert a list of Bits to a list of Bytes of given bytesize, bubblesorting
--   each byte.

-- | ==== __Examples__
--   >>> convertRawBitsToBytes [5,1,3,7,8,2,4,6] 4
--   [[2,4,6,8],[1,3,5,7]]
convertRawBitsToBytes :: [Int] -> Int -> [Byte]
convertRawBitsToBytes bits bytesize = foldr acc [] (splitEvery bytesize bits)
  where
    acc :: [Int] -> [Byte] -> [Byte]
    acc byte bytes = bytes ++ [bubblesort byte]

-- | Convert a list of Bytes to a list of Bytestacks.

-- | This is accomplished by making a Bytestore for each Byte, converting that
--   Bytestore into a Bytestack (these are equivalent terms - see type
--   definitions for more info) and collating the Bytestacks into a list

-- | ==== __Examples__
--  >>> getBytestacksFromBytes [[2,4],[6,8],[1,3],[5,7]] 2
--  [Metabyte ([(0,3),(1,7)],Memory [[1,3],[5,7]]),Metabyte ([(0,4),(1,8)],Memory [[2,4],[6,8]])]
getBytestacksFromBytes :: [Byte] -> Int -> [Bytestack]
getBytestacksFromBytes bytes bytesize = foldr acc [] (splitEvery bytesize bytes)
  where
    acc :: [Byte] -> [Bytestack] -> [Bytestack]
    acc byte bytestacks = bytestacks ++ [Metabyte (getBytestoreFromBytes byte)]

-- | Convert a list of Bytes to a Bytestore.

-- | We do this by loading the list of Bytes into the new Bytestore's Memory
--   and adding a sorted Register containing References to each Byte in Memory.

-- | Each Record contains an Address pointing to the index of the referenced
--   Byte and a TopBit containing the value of the last (i.e. highest) Bit in
--   the referenced Byte.

-- | The Register is bubblesorted by the TopBits of each Record.

-- | ==== __Examples__
--  >>> getBytestoreFromBytes [[2,4,6,8],[1,3,5,7]]
--  ([(0,8),(1,7)],Memory [[2,4,6,8],[1,3,5,7]])
getBytestoreFromBytes :: [Byte] -> Bytestore
getBytestoreFromBytes bytes = do
  let register = acc bytes [] 0
  (register, Memory bytes)
  where
    acc :: [Byte] -> [Record] -> Int -> [Record]
    acc [] register _ = register
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)
