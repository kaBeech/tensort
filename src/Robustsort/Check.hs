module Robustsort.Check (isSorted) where

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (element1 : element2 : remainingElements) = element1 <= element2 && isSorted (element2 : remainingElements)
