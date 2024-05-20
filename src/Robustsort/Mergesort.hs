module Robustsort.Mergesort (mergesort) where

mergesort :: [Int] -> [Int]
mergesort = mergeAll . map (: [])
  where
    mergeAll [] = []
    mergeAll [singleElement] = singleElement
    mergeAll [element1, element2] = merge element1 element2
    mergeAll remaningElements = mergeAll (mergePairs remaningElements)

    mergePairs (element1 : element2 : remaningElements) = merge element1 element2 : mergePairs remaningElements
    mergePairs remaningElements = remaningElements

merge :: [Int] -> [Int] -> [Int]
merge [] element2 = element2
merge element1 [] = element1
merge (element1 : remainingElements1) (element2 : remainingElements2)
  | element1 < element2 = element1 : merge remainingElements1 (element2 : remainingElements2)
  | otherwise = element2 : merge (element1 : remainingElements1) remainingElements2
