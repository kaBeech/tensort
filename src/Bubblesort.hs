module Bubblesort (bubblesort) where

bubblesort :: (Ord array) => [array] -> [array]
bubblesort = foldr bubblesortSinglePass []

bubblesortSinglePass :: (Ord array) => array -> [array] -> [array]
bubblesortSinglePass element1 [] = [element1]
bubblesortSinglePass element1 (element2 : remaningElements) = do
  if element1 < element2
    then element1 : bubblesortSinglePass element2 remaningElements
    else element2 : bubblesortSinglePass element1 remaningElements
