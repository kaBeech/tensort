-- | This module provides the exchangesort function for sorting Sortable lists
module Data.Tensort.Subalgorithms.Exchangesort (exchangesort) where

-- | Takes a Sortable and returns a sorted Sortable using an Exchangesort
--   algorithm.

-- | ==== __Examples__
-- >>> exchangesort (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> exchangesort (SortRec [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)])
-- SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
exchangesort :: (Ord a) => [a] -> [a]
exchangesort bits = exchangesort' bits 0 (length bits - 1)

exchangesort' :: (Ord a) => [a] -> Int -> Int -> [a]
exchangesort' xs i j
  | i > length xs - 1 =
      xs
  | j < 0 =
      exchangesort' xs (i + 1) (length xs - 1)
  | i == j =
      exchangesort' xs i (j - 1)
  | otherwise =
      if leftElemGreater
        then exchangesort' swappedXs i (j - 1)
        else exchangesort' xs i (j - 1)
  where
    mini = min i j
    maxi = max i j
    left = take mini xs
    middle = take (maxi - mini - 1) (drop (mini + 1) xs)
    right = drop (maxi + 1) xs
    x = xs !! mini
    y = xs !! maxi
    leftElemGreater = x > y
    swappedXs = left ++ [y] ++ middle ++ [x] ++ right
