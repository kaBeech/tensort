module Data.Tensort.Subalgorithms.Rotationsort
  ( rotationsort,
    rotationsortAmbi,
    rotationsortReverse,
    rotationsortReverseAmbi,
  )
where

import Data.Tensort.Utils.ComparisonFunctions
  ( greaterThanBit,
    greaterThanRecord,
  )
import Data.Tensort.Utils.Types (Sortable (..))

-- I was having some issues with the swaps for larger input lists, so
-- for now this is only implemented for lists of length 3 or less.

rotationsort :: Sortable -> Sortable
rotationsort (SortBit bits) =
  let result =
        rotationsortIterable greaterThanBit bits 0 False False
   in SortBit result
rotationsort (SortRec recs) =
  let result =
        rotationsortIterable greaterThanRecord recs 0 False False
   in SortRec result

rotationsortAmbi :: Sortable -> Sortable
rotationsortAmbi (SortBit bits) =
  let result =
        rotationsortIterable greaterThanBit bits 0 True False
   in SortBit result
rotationsortAmbi (SortRec recs) =
  let result =
        rotationsortIterable greaterThanRecord recs 0 True False
   in SortRec result

rotationsortReverse :: Sortable -> Sortable
rotationsortReverse (SortBit bits) =
  let result =
        rotationsortIterable
          greaterThanBit
          bits
          (length bits - 1)
          False
          True
   in SortBit result
rotationsortReverse (SortRec recs) =
  let result =
        rotationsortIterable
          greaterThanRecord
          recs
          (length recs - 1)
          False
          True
   in SortRec result

rotationsortReverseAmbi :: Sortable -> Sortable
rotationsortReverseAmbi (SortBit bits) =
  let result =
        rotationsortIterable
          greaterThanBit
          bits
          (length bits - 1)
          True
          True
   in SortBit result
rotationsortReverseAmbi (SortRec recs) =
  let result =
        rotationsortIterable
          greaterThanRecord
          recs
          (length recs - 1)
          True
          True
   in SortRec result

rotationsortIterable ::
  (Ord a) =>
  (a -> a -> Bool) ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  [a]
rotationsortIterable greaterThan xs currentIndex isAmbi isReverse
  | length xs > 3 =
      error
        "From rotationsortIterable: algorithm not yet implemented for lists of length greater than 3"
  | currentIndex < 0 || currentIndex >= length xs =
      xs
  | length xs < 2 = xs
  | length xs == 2 =
      rotatationsortPair greaterThan xs currentIndex isAmbi isReverse
  | currentIndex == firstIndex (length xs) isReverse =
      rotationsortHead greaterThan xs currentIndex isAmbi isReverse
  | currentIndex == lastIndex (length xs) isReverse =
      rotationsortLast greaterThan xs currentIndex isAmbi isReverse
  | otherwise =
      rotationsortMiddle greaterThan xs currentIndex isAmbi isReverse

rotatationsortPair ::
  (Ord a) =>
  (a -> a -> Bool) ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  [a]
rotatationsortPair greaterThan xs currentIndex isAmbi isReverse =
  let x = head xs
      y = xs !! 1
      secondElemGreater = greaterThan y x
      swappedXs = y : [x]
   in switch secondElemGreater swappedXs
  where
    switch secondElemGreater swappedXs
      | not secondElemGreater =
          rotationsortIterable
            greaterThan
            swappedXs
            (firstIndex (length xs) isReverse)
            isAmbi
            isReverse
      | otherwise =
          rotationsortIterable
            greaterThan
            xs
            (nextIndex currentIndex isReverse)
            isAmbi
            isReverse

rotationsortHead ::
  (Ord a) =>
  (a -> a -> Bool) ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  [a]
rotationsortHead greaterThan xs currentIndex isAmbi isReverse =
  let w = xs !! lastIndex (length xs) isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      lastElemGreater = greaterThan w x
      lastElemOrdered =
        if isReverse then not lastElemGreater else lastElemGreater
      nextElemGreater = greaterThan y x
      nextElemOrdered =
        if isReverse then not nextElemGreater else nextElemGreater
      rotateToFirst =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
      rotateBackward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
   in switch
        lastElemOrdered
        nextElemOrdered
        rotateToFirst
        rotateBackward
  where
    switch
      lastElemOrdered
      nextElemOrdered
      rotateToFirst
      rotateBackward
        | not lastElemOrdered =
            rotationsortIterable
              greaterThan
              rotateToFirst
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not nextElemOrdered =
            rotationsortIterable
              greaterThan
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThan
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse

rotationsortMiddle ::
  (Ord a) =>
  (a -> a -> Bool) ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  [a]
rotationsortMiddle greaterThan xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      nextElemGreater = greaterThan y x
      nextElemOrdered =
        if isReverse then not nextElemGreater else nextElemGreater
      prevElemGreater = greaterThan w x
      prevElemOrdered =
        if isReverse then prevElemGreater else not prevElemGreater
      rotateBackward =
        if isReverse then [x] ++ [y] ++ [w] else [y] ++ [w] ++ [x]
      rotateForward =
        if isReverse then [y] ++ [w] ++ [x] else [x] ++ [y] ++ [w]
   in switch
        nextElemOrdered
        prevElemOrdered
        rotateBackward
        rotateForward
  where
    switch
      nextElemOrdered
      prevElemOrdered
      rotateBackward
      rotateForward
        | not nextElemOrdered =
            rotationsortIterable
              greaterThan
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not isAmbi =
            rotationsortIterable
              greaterThan
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not prevElemOrdered =
            rotationsortIterable
              greaterThan
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThan
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse

rotationsortLast ::
  (Ord a) =>
  (a -> a -> Bool) ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  [a]
rotationsortLast greaterThan xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! firstIndex (length xs) isReverse
      firstElemGreater = greaterThan y x
      firstElemOrdered =
        if isReverse then firstElemGreater else not firstElemGreater
      prevElemGreater = greaterThan w x
      prevElemOrdered =
        if isReverse then prevElemGreater else not prevElemGreater
      rotateForward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
      rotateToLast =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
   in switch
        firstElemOrdered
        prevElemOrdered
        rotateForward
        rotateToLast
  where
    switch
      firstElemOrdered
      prevElemOrdered
      rotateForward
      rotateToLast
        | not isAmbi =
            rotationsortIterable
              greaterThan
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not firstElemOrdered =
            rotationsortIterable
              greaterThan
              rotateToLast
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not prevElemOrdered =
            rotationsortIterable
              greaterThan
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThan
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse

nextIndex :: Int -> Bool -> Int
nextIndex currentIndex isReverse
  | isReverse = currentIndex - 1
  | otherwise = currentIndex + 1

prevIndex :: Int -> Bool -> Int
prevIndex currentIndex isReverse
  | isReverse = currentIndex + 1
  | otherwise = currentIndex - 1

lastIndex :: Int -> Bool -> Int
lastIndex listLength isReverse
  | isReverse = 0
  | otherwise = listLength - 1

firstIndex :: Int -> Bool -> Int
firstIndex listLength isReverse
  | isReverse = listLength - 1
  | otherwise = 0
