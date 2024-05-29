module Data.Robustsort.Bytesort (bytesort, bytesort4Bit) where

import Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestacksFromBytes, reduceBytestacks)
import Data.Robustsort.Utils.Bytes2 (getSortedBitsFromMetastack)
import Data.Robustsort.Utils.RandomizeList (randomizeList)

bytesort4Bit :: [Int] -> [Int]
bytesort4Bit xs = bytesort xs 4

bytesort :: [Int] -> Int -> [Int]
bytesort xs bytesize = do
  let bits = randomizeList xs 143
  let bytes = convertRawBitsToBytes bits bytesize
  let bytestacks = getBytestacksFromBytes bytes bytesize
  let metastack = reduceBytestacks bytestacks bytesize
  getSortedBitsFromMetastack metastack
