-- | This module provides variations of the Tensort algorithm using the
--   Sortable type
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
import Data.Tensort.Utils.Types
  ( Sortable (..),
    TensortProps (..),
    WonkyState,
    fromSBitBits,
    fromSBitRecs,
  )

-- | Sort a list of Sortables using a custom Tensort algorithm
--
-- | Takes TensortProps and a Sortable and returns a sorted Sortable

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> let inputList = randomizeList 143 (SortBit [1..100])
-- >>> tensort (mkTsProps 2 bubblesort) inputList
-- SortBit [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
-- >>> tensort (mkTsProps 2 bubblesort) (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensort (mkTsProps 2 bubblesort) (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensort :: TensortProps -> WonkyState -> Sortable -> (Sortable, WonkyState)
tensort _ wonkySt (SortBit []) = (SortBit [], wonkySt)
tensort _ wonkySt (SortBit [x]) = (SortBit [x], wonkySt)
tensort tsProps wonkySt (SortBit [x, y]) =
  subAlgorithm tsProps wonkySt (SortBit [x, y])
tensort tsProps wonkySt (SortBit xs) = do
  let bits = randomizeList 143 (SortBit xs)
  let (bytes, wonkySt') = rawToBytes tsProps wonkySt bits
  let (tensorStacks, wonkySt'') = createInitialTensors tsProps wonkySt' bytes
  let (topTensor, wonkySt''') =
        reduceTensorStacks tsProps wonkySt'' tensorStacks
  let (result, wonkySt'''') =
        getSortedBitsFromTensor (subAlgorithm tsProps) wonkySt''' topTensor
  (fromSBitBits result, wonkySt'''')
tensort _ wonkySt (SortRec []) = (SortRec [], wonkySt)
tensort _ wonkySt (SortRec [x]) = (SortRec [x], wonkySt)
tensort tsProps wonkySt (SortRec [x, y]) =
  subAlgorithm tsProps wonkySt (SortRec [x, y])
tensort tsProps wonkySt (SortRec xs) = do
  let recs = randomizeList 143 (SortRec xs)
  let (bytes, wonkySt') = rawToBytes tsProps wonkySt recs
  let (tensorStacks, wonkySt'') = createInitialTensors tsProps wonkySt' bytes
  let (topTensor, wonkySt''') =
        reduceTensorStacks tsProps wonkySt'' tensorStacks
  let (result, wonkySt'''') =
        getSortedBitsFromTensor (subAlgorithm tsProps) wonkySt''' topTensor
  (fromSBitRecs result, wonkySt'''')

-- | Sort a list of Sortables using a Standard Tensort algorithm with a 4-Bit
--   Bytesize

-- | ==== __Examples__
-- >>> tensortB4 (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensortB4 (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensortB4 :: WonkyState -> Sortable -> (Sortable, WonkyState)
tensortB4 = tensort (mkTsProps 4 bubblesort)

tensortBN :: Int -> WonkyState -> Sortable -> (Sortable, WonkyState)
tensortBN n = tensort (mkTsProps n bubblesort)

tensortBL :: WonkyState -> Sortable -> (Sortable, WonkyState)
tensortBL wonkySt xs =
  tensort (mkTsProps (calculateBytesize xs) bubblesort) wonkySt xs

-- | Calculate a logarithmic Bytesize from a Sortable

-- | ==== __Examples__
-- >>> calculateBytesize (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- 2
calculateBytesize :: Sortable -> Int
calculateBytesize (SortBit xs) =
  ceiling (log (fromIntegral (length xs)) :: Double)
calculateBytesize (SortRec xs) =
  ceiling (log (fromIntegral (length xs)) :: Double)
