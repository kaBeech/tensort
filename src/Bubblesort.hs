module Bubblesort (bsort) where

bsort :: (Ord array) => [array] -> [array]
bsort = foldr bubblesortSinglePass []

bubblesortSinglePass :: (Ord array) => array -> [array] -> [array]
bubblesortSinglePass element1 [] = [element1]
bubblesortSinglePass element1 (element2 : remaningElements) = min element1 element2 : bubblesortSinglePass (max element1 element2) remaningElements
