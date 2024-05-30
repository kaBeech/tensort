module Data.Tensort.Tensort
  ( tensort,
    tensortBasic2Bit,
    tensortBasic3Bit,
    tensortBasic4Bit,
    mkTSProps,
  )
where

import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Utils.Compose (createInitialTensors)
import Data.Tensort.Utils.Convert (rawBitsToBytes)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Reduce (reduceTensorStacks)
import Data.Tensort.Utils.Render (getSortedBitsFromTensor)
import Data.Tensort.Utils.Types (Sortable (..), TensortProps (..), fromSortBit)

mkTSProps :: Int -> (Sortable -> Sortable) -> TensortProps
mkTSProps bSize subAlg = TensortProps {bytesize = bSize, subAlgorithm = subAlg}

tensortBasic2Bit :: [Int] -> [Int]
tensortBasic2Bit xs = tensort xs (mkTSProps 2 bubblesort)

tensortBasic3Bit :: [Int] -> [Int]
tensortBasic3Bit xs = tensort xs (mkTSProps 3 bubblesort)

tensortBasic4Bit :: [Int] -> [Int]
tensortBasic4Bit xs = tensort xs (mkTSProps 4 bubblesort)

-- | Sort a list of Bits using the Tensort algorithm

-- | ==== __Examples__
-- >>> tensort (randomizeList [1..100] 143) 2
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
tensort :: [Int] -> TensortProps -> [Int]
tensort xs tsProps = do
  let bits = randomizeList (SortBit xs) 143
  let bytes = rawBitsToBytes (fromSortBit bits) tsProps
  let tensorStacks = createInitialTensors bytes tsProps
  let topTensor = reduceTensorStacks tensorStacks tsProps
  getSortedBitsFromTensor topTensor (subAlgorithm tsProps)
