module Data.Robustsort.Utils.RandomizeList (randomizeList) where

import Data.Robustsort.Utils.Types (Sortable (..))
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

randomizeList :: Sortable -> Int -> Sortable
randomizeList (SortInt xs) seed = SortInt (shuffle' xs (length xs) (mkStdGen seed))
randomizeList (SortRec xs) seed = SortRec (shuffle' xs (length xs) (mkStdGen seed))
