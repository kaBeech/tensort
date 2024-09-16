-- | This module provides a function to split a list into chunks of a given
--   size.
module Data.Tensort.Utils.Split (splitEvery) where

-- | Split a list into chunks of a given size.
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)
