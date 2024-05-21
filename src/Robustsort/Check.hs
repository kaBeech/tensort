module Robustsort.Check (isSorted) where

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : remainingElements) = x <= y && isSorted (y : remainingElements)
