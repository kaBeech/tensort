-- See `bytesort.ts` for (pseudo)code
module Data.Robustsort.Bytesort (bytesort, bytesort4Bit) where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestacksFromBytes, getSortedArrayFromBytestacks, reduceBytestacks)
import Data.Robustsort.Utils.RandomizeList (randomizeList)

bytesort4Bit :: [Int] -> [Int]
bytesort4Bit xs = bytesort xs 4

bytesort :: [Int] -> Int -> [Int]
bytesort xs bytesize = do
  let bits = randomizeList xs
  let bytes = convertRawBitsToBytes bits bytesize
  let bytestacks = getBytestacksFromBytes bytes bytesize
  let bytestacks' = reduceBytestacks bytestacks bytesize
  getSortedArrayFromBytestacks bytestacks'
