module Data.Robustsort.Subalgorithms.Bubblesort (bubblesort, bubblesortRecords) where

import Data.Robustsort.Utils.ComparisonFunctions (lessThanInt, lessThanRecord)
import Data.Robustsort.Utils.Types (Record)

bubblesort :: [Int] -> [Int]
bubblesort = foldr acc []
  where
    acc :: Int -> [Int] -> [Int]
    acc x xs = bubblesortSinglePass x xs lessThanInt

bubblesortRecords :: [Record] -> [Record]
bubblesortRecords = foldr acc []
  where
    acc :: Record -> [Record] -> [Record]
    acc x xs = bubblesortSinglePass x xs lessThanRecord

bubblesortSinglePass :: a -> [a] -> (a -> a -> Bool) -> [a]
bubblesortSinglePass x [] _ = [x]
bubblesortSinglePass x (y : remaningElements) comparisonFunction = do
  if comparisonFunction x y
    then x : bubblesortSinglePass y remaningElements comparisonFunction
    else y : bubblesortSinglePass x remaningElements comparisonFunction
