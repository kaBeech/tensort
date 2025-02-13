-- | This module provides the bubblesort function for sorting Bit lists
module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

-- | Takes a Bit and returns a sorted Bit using a Bubblesort
--   algorithm.

-- | ==== __Examples__
-- >>> bubblesort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> bubblesort (SortRec [(1, 16), (5, 23), (2, 4), (3, 8), (0, 15), (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
bubblesort :: (Ord a) => [a] -> [a]
bubblesort bits =
  bubblesort' bits 0 (length bits)

bubblesort' :: (Ord a) => [a] -> Int -> Int -> [a]
bubblesort' xs currentIndex i
  | length xs < 2 = xs
  | i < 1 =
      xs
  | currentIndex > length xs - 2 =
      bubblesort' xs 0 (i - 1)
  | otherwise =
      if leftElemGreater
        then bubblesort' swappedXs (currentIndex + 1) i
        else bubblesort' xs (currentIndex + 1) i
  where
    left = take currentIndex xs
    right = drop (currentIndex + 2) xs
    x = xs !! currentIndex
    y = xs !! (currentIndex + 1)
    leftElemGreater = x > y
    swappedXs = left ++ [y] ++ [x] ++ right
