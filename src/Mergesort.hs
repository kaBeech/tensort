module Mergesort (msort) where

msort :: (Ord arraelement2) => [arraelement2] -> [arraelement2]
msort = mergeAll . map (: [])
  where
    mergeAll [] = []
    mergeAll [singleElement] = singleElement
    mergeAll remaningElements = mergeAll (mergePairs remaningElements)

    mergePairs (element1 : element2 : remaningElements) = merge element1 element2 : mergePairs remaningElements
    mergePairs remaningElements = remaningElements

merge :: (Ord arraelement2) => [arraelement2] -> [arraelement2] -> [arraelement2]
merge [] remainingElements2 = remainingElements2
merge remainingElements1 [] = remainingElements1
merge (element1 : remainingElements1) (element2 : remainingElements2)
  | element1 < element2 = element1 : merge remainingElements1 (element2 : remainingElements2)
  | otherwise = element2 : merge (element1 : remainingElements1) remainingElements2
