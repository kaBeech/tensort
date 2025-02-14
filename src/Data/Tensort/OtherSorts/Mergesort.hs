-- | This module provides an implementation of the mergesort algorithm.
module Data.Tensort.OtherSorts.Mergesort (mergesort) where

-- | Takes a list and returns a sorted list using a Mergesort
--   algorithm.

-- | ==== __Examples__
--  >>> mergesort [16, 23, 4, 8, 15, 42]
--  [4,8,15,16,23,42]
--
--  >>> mergesort [(1, 16), (5, 23), (2, 4) ,(3, 8), (0, 15) , (4, 42)]
--  [(2,4),(3,8),(0,15),(1,16),(5,23),(4,42)]
mergesort :: (Ord a) => [a] -> [a]
mergesort = mergesortStack . map (: [])

mergesortStack :: (Ord a) => [[a]] -> [a]
mergesortStack [] = []
mergesortStack [l] = l
mergesortStack [l1, l2] = mergeLists l1 l2
mergesortStack remaningElements =
  mergesortStack
    (mergePairsOfLists remaningElements)

mergePairsOfLists :: (Ord a) => [[a]] -> [[a]]
mergePairsOfLists [] = []
mergePairsOfLists [l1] = [l1]
mergePairsOfLists [l1, l2] = [mergeLists l1 l2]
mergePairsOfLists (l1 : l2 : remaningElements) =
  mergeLists l1 l2 : mergePairsOfLists remaningElements

mergeLists :: (Ord a) => [a] -> [a] -> [a]
mergeLists [] y = y
mergeLists x [] = x
mergeLists (x : xs) (y : ys)
  | x < y = x : mergeLists xs (y : ys)
  | otherwise = y : mergeLists (x : xs) ys
