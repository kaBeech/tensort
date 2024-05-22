module Data.Robustsort.Utils.RandomizeList (randomizeList) where

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

randomizeList :: [Int] -> Int -> [Int]
randomizeList xs seed = shuffle' xs (length xs) (mkStdGen seed)
