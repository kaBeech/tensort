module Data.Tensort.Utils.RandomizeList (randomizeList) where

import Data.Tensort.Utils.Types (Sortable (..))
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

randomizeList :: Int -> Sortable -> Sortable
randomizeList seed (SortBit xs) = SortBit (shuffle' xs (length xs) (mkStdGen seed))
randomizeList seed (SortRec xs) = SortRec (shuffle' xs (length xs) (mkStdGen seed))
