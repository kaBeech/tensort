module Data.Tensort.OtherSorts.Mergesort (mergesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanInt, lessThanRecord)
import Data.Tensort.Utils.Types (Record, Sortable (..))

mergesort :: Sortable -> Sortable
mergesort (SortInt xs) = SortInt (mergesortInts xs)
mergesort (SortRec xs) = SortRec (mergesortRecs xs)

mergesortInts :: [Int] -> [Int]
mergesortInts = mergeAllInts . map (: [])
  where
    mergeAllInts [] = []
    mergeAllInts [x] = x
    mergeAllInts [x, y] = mergeInts x y
    mergeAllInts remaningElements = mergeAllInts (mergePairs remaningElements)

    mergePairs (x : y : remaningElements) = mergeInts x y : mergePairs remaningElements
    mergePairs x = x

mergeInts :: [Int] -> [Int] -> [Int]
mergeInts [] y = y
mergeInts x [] = x
mergeInts (x : xs) (y : ys)
  | lessThanInt x y = x : mergeInts xs (y : ys)
  | otherwise = y : mergeInts (x : xs) ys

mergesortRecs :: [Record] -> [Record]
mergesortRecs = mergeAllRecs . map (: [])
  where
    mergeAllRecs [] = []
    mergeAllRecs [x] = x
    mergeAllRecs [x, y] = mergeRecs x y
    mergeAllRecs remaningElements = mergeAllRecs (mergePairs remaningElements)

    mergePairs (x : y : remaningElements) = mergeRecs x y : mergePairs remaningElements
    mergePairs x = x

mergeRecs :: [Record] -> [Record] -> [Record]
mergeRecs [] y = y
mergeRecs x [] = x
mergeRecs (x : xs) (y : ys)
  | lessThanRecord x y = x : mergeRecs xs (y : ys)
  | otherwise = y : mergeRecs (x : xs) ys
