module Data.Robustsort.Bytesort (bytesort, bytesort4Bit) where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Utils.Bytes (BytesortProps, convertRawBitsToBytes, getBytestacksFromBytes, getSortedBitsFromMetastack, mkBSProps, reduceBytestacks)
import Data.Robustsort.Utils.RandomizeList (randomizeList)

bytesort4Bit :: [Int] -> [Int]
bytesort4Bit xs = bytesort xs (mkBSProps 4 bubblesort)

-- | Sort a list of Ints using the Bytesort algorithm

-- | ==== __Examples__
-- >>> bytesort [14, 7, 38, 29, 56, 23, 42, 69, 50, 20, 11, 44, 17, 62, 35, 2, 47, 26, 59, 32] 2
-- [2,7,11,14,17,20,23,26,29,32,35,38,42,44,47,50,56,59,62,69]
-- >>> bytesort [14, 7, 38, 29, 56, 23, 42, 69, 50, 20, 11, 44, 17, 62, 35, 2, 47, 26, 59, 32] 4
-- [2,7,11,14,17,20,23,26,29,32,35,38,42,44,47,50,56,59,62,69]
-- >>> bytesort [14, 7, 38, 29, 56, 23, 42, 69, 50, 20, 11, 44, 17, 62, 35, 2, 47, 26, 59, 32] 8
-- [2,7,11,14,17,20,23,26,29,32,35,38,42,44,47,50,56,59,62,69]
-- >>> bytesort (randomizeList [1..100] 143) 2
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
bytesort :: [Int] -> BytesortProps -> [Int]
bytesort xs bsProps = do
  let bits = randomizeList xs 143
  let bytes = convertRawBitsToBytes bits bsProps
  let bytestacks = getBytestacksFromBytes bytes bsProps
  let metastack = reduceBytestacks bytestacks bsProps
  getSortedBitsFromMetastack metastack
