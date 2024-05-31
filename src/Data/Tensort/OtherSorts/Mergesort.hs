module Data.Tensort.OtherSorts.Mergesort (mergesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Record, Sortable (..), Bit)

mergesort :: Sortable -> Sortable
mergesort (SortBit xs) = SortBit (mergesortBits xs)
mergesort (SortRec xs) = SortRec (mergesortRecs xs)

mergesortBits :: [Bit] -> [Bit]
mergesortBits = mergeAllBits . map (: [])
  where
    mergeAllBits [] = []
    mergeAllBits [x] = x
    mergeAllBits [x, y] = mergeBits x y
    mergeAllBits remaningElements = mergeAllBits (mergePairs remaningElements)

    mergePairs (x : y : remaningElements) = mergeBits x y : mergePairs remaningElements
    mergePairs x = x

mergeBits :: [Bit] -> [Bit] -> [Bit]
mergeBits [] y = y
mergeBits x [] = x
mergeBits (x : xs) (y : ys)
  | lessThanBit x y = x : mergeBits xs (y : ys)
  | otherwise = y : mergeBits (x : xs) ys

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
