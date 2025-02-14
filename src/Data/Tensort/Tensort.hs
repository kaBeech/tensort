-- | This module provides variations of the Tensort algorithm using the
--   custom Sortable type for inputs and outputs
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
import Data.Tensort.Utils.LogNat (getLnBytesize)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Reduce (reduceTensorStacks)
import Data.Tensort.Utils.Render (getSortedBits)
import Data.Tensort.Utils.Types
  ( Bit,
    TensortProps (..),
  )

-- | Sort a Sortable list using a custom Tensort algorithm
--
--   Takes TensortProps (Bytesize and SubAlgorithm) and a Sortable and returns
--   a sorted Sortable

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> tensort (mkTsProps 2 bubblesort) (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensort (mkTsProps 2 bubblesort) (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensort :: (Ord a) => TensortProps a -> [Bit a] -> [Bit a]
tensort _ [] = []
tensort _ [x] = [x]
tensort tsProps [x, y] = subAlgorithm tsProps [x, y]
tensort tsProps xs = getSortedBits subAlg topTensor
  where
    subAlg = subAlgorithm tsProps
    topTensor = reduceTensorStacks tsProps tensorStacks
    tensorStacks = createInitialTensors tsProps bytes
    bytes = rawBitsToBytes tsProps bits
    bits = randomizeList 143 xs

-- | Sort a Sortable list using a Standard Tensort algorithm with a 4-Bit
--   Bytesize

-- | ==== __Examples__
-- >>> tensortB4 (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensortB4 (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensortB4 :: (Ord a) => [Bit a] -> [Bit a]
tensortB4 = tensort $ mkTsProps 4 bubblesort

-- | Sort a Sortable list using a Standard Tensort algorithm with a custom
--   Bytesize

-- | ==== __Examples__
-- >>> tensortBN 3 (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensortBN 3 (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensortBN :: (Ord a) => Int -> [Bit a] -> [Bit a]
tensortBN n = tensort $ mkTsProps n bubblesort

-- | Sort a Sortable list using a Standard Logarithmic Tensort algorithm
--
--   Standard Logarithmic Tensort uses a Bytesize that approximates the natural
--   logarithm of the length of the input list and a Bubblesort subalgorithm

-- | ==== __Examples__
-- >>> tensortBL (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> tensortBL (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
tensortBL :: (Ord a) => [Bit a] -> [Bit a]
tensortBL xs = tensort tsProps xs
  where
    tsProps = mkTsProps (getLnBytesize xs) bubblesort
