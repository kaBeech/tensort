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
import Data.Tensort.Utils.LogNat (getLnBytesize)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Reduce (reduceTensorStacks)
import Data.Tensort.Utils.Render (getSortedBitsFromTensor)
import Data.Tensort.Utils.Types
  ( Sortable (..),
    TensortProps (..),
    fromSBitBits,
    fromSBitRecs,
  )

-- | Sort a Sortable list using a custom Tensort algorithm
--
--   Takes TensortProps and a Sortable and returns a sorted Sortable

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> tensort (mkTsProps 2 bubblesort) (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensort (mkTsProps 2 bubblesort) (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensort :: TensortProps -> Sortable -> Sortable
tensort _ (SortBit []) = SortBit []
tensort _ (SortBit [x]) = SortBit [x]
tensort tsProps (SortBit [x, y]) = subAlgorithm tsProps (SortBit [x, y])
tensort tsProps (SortBit xs) = do
  let bits = randomizeList 143 (SortBit xs)
  let bytes = rawToBytes tsProps bits
  let tensorStacks = createInitialTensors tsProps bytes
  let topTensor = reduceTensorStacks tsProps tensorStacks
  fromSBitBits (getSortedBitsFromTensor (subAlgorithm tsProps) topTensor)
tensort _ (SortRec []) = SortRec []
tensort _ (SortRec [x]) = SortRec [x]
tensort tsProps (SortRec [x, y]) = subAlgorithm tsProps (SortRec [x, y])
tensort tsProps (SortRec xs) = do
  let recs = randomizeList 143 (SortRec xs)
  let bytes = rawToBytes tsProps recs
  let tensorStacks = createInitialTensors tsProps bytes
  let topTensor = reduceTensorStacks tsProps tensorStacks
  fromSBitRecs (getSortedBitsFromTensor (subAlgorithm tsProps) topTensor)

-- | Sort a Sortable list using a Standard Tensort algorithm with a 4-Bit
--   Bytesize

-- | ==== __Examples__
-- >>> tensortB4 (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensortB4 (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensortB4 :: Sortable -> Sortable
tensortB4 = tensort (mkTsProps 4 bubblesort)

-- | Sort a Sortable list using a Standard Tensort algorithm with a custom
--   Bytesize

-- | ==== __Examples__
-- >>> tensortBN 3 (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensortBN 3 (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensortBN :: Int -> Sortable -> Sortable
tensortBN n = tensort (mkTsProps n bubblesort)

-- | Sort a Sortable list using a Standard Logarithmic Tensort algorithm

-- | ==== __Examples__
-- >>> tensortBL (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensortBL (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensortBL :: Sortable -> Sortable
tensortBL xs = tensort (mkTsProps (getLnBytesize xs) bubblesort) xs
