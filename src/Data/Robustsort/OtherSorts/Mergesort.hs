module Data.Robustsort.OtherSorts.Mergesort (mergesort) where

mergesort :: [Int] -> [Int]
mergesort = mergeAll . map (: [])
  where
    mergeAll [] = []
    mergeAll [x] = x
    mergeAll [x, y] = merge x y
    mergeAll remaningElements = mergeAll (mergePairs remaningElements)

    mergePairs (x : y : remaningElements) = merge x y : mergePairs remaningElements
    mergePairs x = x

merge :: [Int] -> [Int] -> [Int]
merge [] y = y
merge x [] = x
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys
