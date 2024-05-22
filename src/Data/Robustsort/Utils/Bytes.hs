-- module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestacksFromBytes, getSortedArrayFromBytestacks, reduceBytestacks) where
module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes) where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Utils.Split (splitEvery)
import Data.Robustsort.Utils.Types (Byte)

-- export default (bits: number[], bytesize: number): number[] => {
--     bits = randomizeArray(bits)
--     const bytes = convertRawBitsToBytes(bits, bytesize)
--     let bytestacks = getBytestacksFromBytes(bytes, bytesize)
--     while (bytestacks.length > bytesize) {
--         bytestacks = reduceBytestacks(bytestacks, bytesize)
--     }
--     return getSortedArrayFromBytestacks(bytestacks)
-- }

-- | Convert a list of bits to a list of bytes, bubblesorting each byte.
convertRawBitsToBytes :: [Int] -> Int -> [Byte]
convertRawBitsToBytes bits bytesize = foldr acc [] (splitEvery bytesize bits)
  where
    acc :: [Int] -> [Byte] -> [Byte]
    acc byte bytes = bytes ++ [bubblesort byte]
