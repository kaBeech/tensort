-- | This module prvodies the randomizeList function, which randomizes the
--   order of elements in sortable lists.
module Data.Tensort.Utils.RandomizeList (randomizeList) where

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

-- | Takes a seed for random generation and a list and returns a new
--   list with the same elements as the input list but in a random order.

-- | ==== __Examples__
-- >>> randomizeList 143 (SortBit [4, 8, 15, 16, 23, 42])
-- SortBit [16,23,4,8,15,42]
--
-- >>> randomizeList 143 (SortRec [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)])
-- SortRec [(1,16),(5,23),(2,4),(3,8),(0,15),(4,42)]
randomizeList :: (Ord a) => Int -> [a] -> [a]
randomizeList seed xs = shuffle' xs (length xs) $ mkStdGen seed
