module Data.Tensort.Tensort
  ( tensort,
    tensortB4,
    tensortBN,
    tensortBL,
  )
where

import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Utils.Compose (createInitialTensors)
import Data.Tensort.Utils.Convert (rawBitsToBytes)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Reduce (reduceTensorStacks)
import Data.Tensort.Utils.Render (getSortedBitsFromTensor)
import Data.Tensort.Utils.Types (Bit, Sortable (..), TensortProps (..), fromSortBit, WonkyState)

-- | Sort a list of Bits using the Tensort algorithm

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> let inputList = fromSortBit (randomizeList (SortBit [1..100]) 143)
-- >>> tensort inputList (mkTsProps 2 bubblesort)
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
tensort :: [Bit] -> TensortProps -> WonkyState -> ([Bit], WonkyState)
tensort [] _ wonkySt = ([], wonkySt)
tensort xs tsProps wonkySt = do
  let bits = randomizeList (SortBit xs) 143
  let (bytes, wonkySt') = rawBitsToBytes (fromSortBit bits) tsProps wonkySt
  let (tensorStacks, wonkySt'') = createInitialTensors bytes tsProps wonkySt'
  let (topTensor, wonkySt''') = reduceTensorStacks tensorStacks tsProps wonkySt''
  getSortedBitsFromTensor topTensor (subAlgorithm tsProps) wonkySt'''

tensortB4 :: [Bit] -> WonkyState -> ([Bit], WonkyState)
tensortB4 xs wonkySt = tensort xs (mkTsProps 4 bubblesort) wonkySt

tensortBN :: Int -> [Bit] -> WonkyState -> ([Bit], WonkyState)
tensortBN n xs wonkySt = tensort xs (mkTsProps n bubblesort) wonkySt

tensortBL :: [Bit] -> WonkyState -> ([Bit], WonkyState)
tensortBL [] wonkySt = ([], wonkySt)
tensortBL [x] wonkySt = ([x], wonkySt)
tensortBL [x, y] wonkySt = if x <= y then ([x, y], wonkySt) else ([y, x], wonkySt)
tensortBL xs wonkySt = tensort xs (mkTsProps (calculateBytesize xs) bubblesort) wonkySt

calculateBytesize :: [Bit] -> Int
calculateBytesize xs = ceiling (log (fromIntegral (length xs)) :: Double)
