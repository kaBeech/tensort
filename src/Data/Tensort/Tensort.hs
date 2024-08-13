module Data.Tensort.Tensort
  ( tensort,
    tensortB4,
    tensortBN,
    tensortBL,
  )
where

import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
import Data.Tensort.Utils.Compose (createInitialTensors)
import Data.Tensort.Utils.Convert (rawToBytes)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Reduce (reduceTensorStacks)
import Data.Tensort.Utils.Render (getSortedBitsFromTensor)
import Data.Tensort.Utils.Types (Sortable (..), TensortProps (..), WonkyState, fromSBitBits, fromSBitRecs)

-- | Sort a list of Bits using the Tensort algorithm

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> let inputList = fromSortBit (randomizeList 143 (SortBit [1..100]) )
-- >>> tensort (mkTsProps 2 bubblesort) inputList
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
tensort :: TensortProps -> WonkyState -> Sortable -> (Sortable, WonkyState)
tensort _ wonkySt (SortBit []) = (SortBit [], wonkySt)
tensort tsProps wonkySt (SortBit xs) = do
  let bits = randomizeList 143 (SortBit xs)
  let (bytes, wonkySt') = rawToBytes tsProps wonkySt bits
  let (tensorStacks, wonkySt'') = createInitialTensors tsProps wonkySt' bytes
  let (topTensor, wonkySt''') = reduceTensorStacks tsProps wonkySt'' tensorStacks
  let (result, wonkySt'''') = getSortedBitsFromTensor (subAlgorithm tsProps) wonkySt''' topTensor
  (fromSBitBits result, wonkySt'''')
tensort _ wonkySt (SortRec []) = (SortRec [], wonkySt)
tensort tsProps wonkySt (SortRec xs) = do
  let recs = randomizeList 143 (SortRec xs)
  let (bytes, wonkySt') = rawToBytes tsProps wonkySt recs
  let (tensorStacks, wonkySt'') = createInitialTensors tsProps wonkySt' bytes
  let (topTensor, wonkySt''') = reduceTensorStacks tsProps wonkySt'' tensorStacks
  let (result, wonkySt'''') = getSortedBitsFromTensor (subAlgorithm tsProps) wonkySt''' topTensor
  (fromSBitRecs result, wonkySt'''')

tensortB4 :: WonkyState -> Sortable -> (Sortable, WonkyState)
tensortB4 = tensort (mkTsProps 4 bubblesort)

tensortBN :: Int -> WonkyState -> Sortable -> (Sortable, WonkyState)
tensortBN n = tensort (mkTsProps n bubblesort)

tensortBL :: WonkyState -> Sortable -> (Sortable, WonkyState)
tensortBL wonkySt (SortBit []) = (SortBit [], wonkySt)
tensortBL wonkySt (SortBit [x]) = (SortBit [x], wonkySt)
tensortBL wonkySt (SortBit [x, y]) = if x <= y then (SortBit [x, y], wonkySt) else (SortBit [y, x], wonkySt)
tensortBL wonkySt xs = tensort (mkTsProps (calculateBytesize xs) bubblesort) wonkySt xs

calculateBytesize :: Sortable -> Int
calculateBytesize (SortBit xs) = ceiling (log (fromIntegral (length xs)) :: Double)
calculateBytesize (SortRec xs) = ceiling (log (fromIntegral (length xs)) :: Double)
