module Data.Tensort.Utils.Score (getTotalPositionalErrors) where

getTotalPositionalErrors :: [Int] -> Int
getTotalPositionalErrors bits = foldr acc 0 (zip [1 ..] bits)
  where
    acc :: (Int, Int) -> Int -> Int
    acc (i, bit) total = total + abs (i - bit)
