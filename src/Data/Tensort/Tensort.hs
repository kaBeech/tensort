-- | This module provides variations of the Tensort algorithm
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
import Data.Tensort.Utils.LogNat (getLnLength)
import Data.Tensort.Utils.MkTsProps (mkTsProps)
import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Reduce (reduceTensorStacks)
import Data.Tensort.Utils.Render (getSortedBits)
import Data.Tensort.Utils.Types
  ( Bit,
    TensortProps (..),
  )

-- | Sort a list using a custom Tensort algorithm
--
--   Takes TensortProps (Bytesize and SubAlgorithm) and a list and returns
--   a sorted list

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> tensort (mkTsProps 2 bubblesort) ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> tensort (mkTsProps 2 bubblesort) ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
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

-- | Sort a list using a Standard Tensort algorithm with a 4-Bit
--   Bytesize

-- | ==== __Examples__
-- >>> tensortB4 ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> tensortB4 ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
tensortB4 :: (Ord a) => [Bit a] -> [Bit a]
tensortB4 = tensort $ mkTsProps 4 bubblesort

-- | Sort a list using a Standard Tensort algorithm with a custom
--   Bytesize

-- | ==== __Examples__
-- >>> tensortBN 3 ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> tensortBN 3 ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
tensortBN :: (Ord a) => Int -> [Bit a] -> [Bit a]
tensortBN n = tensort $ mkTsProps n bubblesort

-- | Sort a list using a Standard Logarithmic Tensort algorithm
--
--   Standard Logarithmic Tensort uses a Bytesize that approximates the natural
--   logarithm of the length of the input list and a Bubblesort subalgorithm

-- | ==== __Examples__
-- >>> tensortBL ([16, 23, 4, 8, 15, 42] :: [Int])
-- [4,8,15,16,23,42]
--
-- >>> tensortBL ([(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)] :: [(Int, Int)])
-- [(0,15),(1,16),(2,4),(3,8),(4,42),(5,23)]
tensortBL :: (Ord a) => [Bit a] -> [Bit a]
tensortBL xs = tensort tsProps xs
  where
    tsProps = mkTsProps (getLnLength xs) bubblesort
