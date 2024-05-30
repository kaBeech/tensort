module Data.Tensort.Utils.RandomizeList (randomizeList) where

import Data.Tensort.Utils.Types (Sortable (..))
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

randomizeList :: Sortable -> Int -> Sortable
randomizeList (SortBit xs) seed = SortBit (shuffle' xs (length xs) (mkStdGen seed))
randomizeList (SortRec xs) seed = SortRec (shuffle' xs (length xs) (mkStdGen seed))
