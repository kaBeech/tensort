module Data.Robustsort.Subalgorithms.Bubblesort (bubblesort) where

bubblesort :: [Int] -> [Int]
bubblesort = foldr bubblesortSinglePass []

bubblesortSinglePass :: Int -> [Int] -> [Int]
bubblesortSinglePass x [] = [x]
bubblesortSinglePass x (y : remaningElements) = do
  if x < y
    then x : bubblesortSinglePass y remaningElements
    else y : bubblesortSinglePass x remaningElements
