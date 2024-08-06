module Data.Tensort.Tensort
  ( tensort,
    tensortB4,
    tensortBN,
    tensortBL,
    mkTsProps,
  )
where

import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Utils.Compose (createInitialTensors)
import Data.Tensort.Utils.Convert (rawBitsToBytes)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Reduce (reduceTensorStacks)
import Data.Tensort.Utils.Render (getSortedBitsFromTensor)
import Data.Tensort.Utils.Types (Bit, SortAlg, Sortable (..), TensortProps (..), fromSortBit)

-- | Sort a list of Bits using the Tensort algorithm

-- | ==== __Examples__
-- >>> tensort (randomizeList [1..100] 143) 2
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
tensort :: [Bit] -> TensortProps -> [Bit]
tensort [] _ = []
tensort xs tsProps = do
  let bits = randomizeList (SortBit xs) 143
  let bytes = rawBitsToBytes (fromSortBit bits) tsProps
  let tensorStacks = createInitialTensors bytes tsProps
  let topTensor = reduceTensorStacks tensorStacks tsProps
  getSortedBitsFromTensor topTensor (subAlgorithm tsProps)

mkTsProps :: Int -> SortAlg -> TensortProps
mkTsProps bSize subAlg = TensortProps {bytesize = bSize, subAlgorithm = subAlg}

tensortB4 :: [Bit] -> [Bit]
tensortB4 xs = tensort xs (mkTsProps 4 bubblesort)

tensortBN :: Int -> [Bit] -> [Bit]
tensortBN n xs = tensort xs (mkTsProps n bubblesort)

tensortBL :: [Bit] -> [Bit]
tensortBL [] = []
tensortBL [x] = [x]
tensortBL [x, y] = if x <= y then [x, y] else [y, x]
tensortBL xs = tensort xs (mkTsProps (calculateBytesize xs) bubblesort)

calculateBytesize :: [Bit] -> Int
calculateBytesize xs = ceiling (log (fromIntegral (length xs)) :: Double)
