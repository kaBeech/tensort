-- module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestacksFromBytes, getSortedArrayFromBytestacks, reduceBytestacks) where
module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestoreFromBytes) where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Utils.Split (splitEvery)
import Data.Robustsort.Utils.Types (Byte, Bytestore, Memory (..), Reference)

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
convertRawBitsToBytes bits bytesize = foldr (\byte bytes -> bytes ++ [bubblesort byte]) [] (splitEvery bytesize bits)

-- | Convert a list of Bytes to a Bytestore.

-- | A Bytestore has two elements: its Register and its Memory.

-- | The Memory is just a list of the bytes that the Bytestore contains.

-- | The Register is a list of References, each containing an Address and a
--   TopBit

-- | A Reference's Address is an index number pointing to a Byte in the Memory

-- | A Reference's TopBit is the last (i.e. highest) Bit in the Byte that the
--   Reference points to

-- | ==== __Examples__
--  >>> getBytestoreFromBytes [[2,4,6,8],[1,3,5,7]]
--  ([(0,8),(1,7)],Memory [[2,4,6,8],[1,3,5,7]])
getBytestoreFromBytes :: [Byte] -> Bytestore
getBytestoreFromBytes bytes = do
  let register = acc bytes [] 0
  (register, Memory bytes)
  where
    acc :: [Byte] -> [Reference] -> Int -> [Reference]
    acc [] register _ = register
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)
