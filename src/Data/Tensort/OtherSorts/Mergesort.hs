module Data.Tensort.OtherSorts.Mergesort (mergesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Bit, Record, Sortable (..), WonkyState)

mergesort :: Sortable -> WonkyState -> (Sortable, WonkyState)
mergesort (SortBit xs) wonkySt = do
  let (result, wonkySt') = mergesortBits xs wonkySt
  (SortBit result, wonkySt')
mergesort (SortRec xs) wonkySt = do
  let (result, wonkySt') = mergesortRecs xs wonkySt
  (SortRec result, wonkySt')

mergesortBits :: [Bit] -> WonkyState -> ([Bit], WonkyState)
mergesortBits [] wonkySt = ([], wonkySt)
mergesortBits [x] wonkySt = ([x], wonkySt)
mergesortBits xs wonkySt = do
  let (left, right) = splitAt (length xs `div` 2) xs
  let (leftSorted, wonkySt') = mergesortBits left wonkySt
  let (rightSorted, wonkySt'') = mergesortBits right wonkySt'
  mergeBits leftSorted rightSorted wonkySt''

mergeBits :: [Bit] -> [Bit] -> WonkyState -> ([Bit], WonkyState)
mergeBits [] y wonkySt = (y, wonkySt)
mergeBits x [] wonkySt = (x, wonkySt)
mergeBits (x : xs) (y : ys) wonkySt = do
  let (firstElemLess, wonkySt') = lessThanBit x y wonkySt
  if firstElemLess
    then do
      let (result, wonkySt'') = mergeBits xs (y : ys) wonkySt'
      (x : result, wonkySt'')
    else do
      let (result, wonkySt'') = mergeBits (x : xs) ys wonkySt'
      (y : result, wonkySt'')

mergesortRecs :: [Record] -> WonkyState -> ([Record], WonkyState)
mergesortRecs recs wonkySt = do
  let (left, right) = splitAt (length recs `div` 2) recs
  let (leftSorted, wonkySt') = mergesortRecs left wonkySt
  let (rightSorted, wonkySt'') = mergesortRecs right wonkySt'
  mergeRecs leftSorted rightSorted wonkySt''

mergeRecs :: [Record] -> [Record] -> WonkyState -> ([Record], WonkyState)
mergeRecs [] y wonkySt = (y, wonkySt)
mergeRecs x [] wonkySt = (x, wonkySt)
mergeRecs (x : xs) (y : ys) wonkySt = do
  let (firstElemLess, wonkySt') = lessThanRecord x y wonkySt
  if firstElemLess
    then do
      let (result, wonkySt'') = mergeRecs xs (y : ys) wonkySt'
      (x : result, wonkySt'')
    else do
      let (result, wonkySt'') = mergeRecs (x : xs) ys wonkySt'
      (y : result, wonkySt'')
