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
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

-- I was having some issues with the swaps for larger input lists, so
-- for now this is only implemented for lists of length 3 or less.

rotationsort :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsort wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanBit wonkySt bits 0 False False
   in (SortBit result, wonkySt')
rotationsort wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanRecord wonkySt recs 0 False False
   in (SortRec result, wonkySt')

rotationsortAmbi :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortAmbi wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanBit wonkySt bits 0 True False
   in (SortBit result, wonkySt')
rotationsortAmbi wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanRecord wonkySt recs 0 True False
   in (SortRec result, wonkySt')

rotationsortReverse :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortReverse wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanBit
          wonkySt
          bits
          (length bits - 1)
          False
          True
   in (SortBit result, wonkySt')
rotationsortReverse wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanRecord
          wonkySt
          recs
          (length recs - 1)
          False
          True
   in (SortRec result, wonkySt')

rotationsortReverseAmbi :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortReverseAmbi wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanBit
          wonkySt
          bits
          (length bits - 1)
          True
          True
   in (SortBit result, wonkySt')
rotationsortReverseAmbi wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanRecord
          wonkySt
          recs
          (length recs - 1)
          True
          True
   in (SortRec result, wonkySt')

rotationsortIterable ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  ([a], WonkyState)
rotationsortIterable greaterThan wonkySt xs currentIndex isAmbi isReverse
  | length xs > 3 =
      error
        "From rotationsortIterable: algorithm not yet implemented for lists of length greater than 3"
  | currentIndex < 0 || currentIndex >= length xs =
      (xs, wonkySt)
  | length xs < 2 = (xs, wonkySt)
  | length xs == 2 =
      rotatationsortPair greaterThan wonkySt xs currentIndex isAmbi isReverse
  | currentIndex == firstIndex (length xs) isReverse =
      rotationsortHead greaterThan wonkySt xs currentIndex isAmbi isReverse
  | currentIndex == lastIndex (length xs) isReverse =
      rotationsortLast greaterThan wonkySt xs currentIndex isAmbi isReverse
  | otherwise =
      rotationsortMiddle greaterThan wonkySt xs currentIndex isAmbi isReverse

rotatationsortPair ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  ([a], WonkyState)
rotatationsortPair greaterThan wonkySt xs currentIndex isAmbi isReverse =
  let x = head xs
      y = xs !! 1
      (secondElemGreater, wonkySt') = greaterThan y x wonkySt
      swappedXs = y : [x]
   in switch secondElemGreater wonkySt' swappedXs
  where
    switch secondElemGreater wonkySt' swappedXs
      | not secondElemGreater =
          rotationsortIterable
            greaterThan
            wonkySt'
            swappedXs
            (firstIndex (length xs) isReverse)
            isAmbi
            isReverse
      | otherwise =
          rotationsortIterable
            greaterThan
            wonkySt'
            xs
            (nextIndex currentIndex isReverse)
            isAmbi
            isReverse

rotationsortHead ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  ([a], WonkyState)
rotationsortHead greaterThan wonkySt xs currentIndex isAmbi isReverse =
  let w = xs !! lastIndex (length xs) isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      (lastElemGreater, wonkySt') = greaterThan w x wonkySt
      lastElemOrdered =
        if isReverse then not lastElemGreater else lastElemGreater
      (nextElemGreater, wonkySt'') = greaterThan y x wonkySt'
      nextElemOrdered =
        if isReverse then not nextElemGreater else nextElemGreater
      rotateToFirst =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
      rotateBackward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
   in switch
        lastElemOrdered
        nextElemOrdered
        wonkySt'
        wonkySt''
        rotateToFirst
        rotateBackward
  where
    switch
      lastElemOrdered
      nextElemOrdered
      wonkySt'
      wonkySt''
      rotateToFirst
      rotateBackward
        | not lastElemOrdered =
            rotationsortIterable
              greaterThan
              wonkySt'
              rotateToFirst
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not nextElemOrdered =
            rotationsortIterable
              greaterThan
              wonkySt''
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThan
              wonkySt''
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse

rotationsortMiddle ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  ([a], WonkyState)
rotationsortMiddle greaterThan wonkySt xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      (nextElemGreater, wonkySt') = greaterThan y x wonkySt
      nextElemOrdered =
        if isReverse then not nextElemGreater else nextElemGreater
      (prevElemGreater, wonkySt'') = greaterThan w x wonkySt'
      prevElemOrdered =
        if isReverse then prevElemGreater else not prevElemGreater
      rotateBackward =
        if isReverse then [x] ++ [y] ++ [w] else [y] ++ [w] ++ [x]
      rotateForward =
        if isReverse then [y] ++ [w] ++ [x] else [x] ++ [y] ++ [w]
   in switch
        nextElemOrdered
        prevElemOrdered
        wonkySt'
        wonkySt''
        rotateBackward
        rotateForward
  where
    switch
      nextElemOrdered
      prevElemOrdered
      wonkySt'
      wonkySt''
      rotateBackward
      rotateForward
        | not nextElemOrdered =
            rotationsortIterable
              greaterThan
              wonkySt'
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not isAmbi =
            rotationsortIterable
              greaterThan
              wonkySt'
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not prevElemOrdered =
            rotationsortIterable
              greaterThan
              wonkySt''
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThan
              wonkySt''
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse

rotationsortLast ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  ([a], WonkyState)
rotationsortLast greaterThan wonkySt xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! firstIndex (length xs) isReverse
      (firstElemGreater, wonkySt') = greaterThan y x wonkySt
      firstElemOrdered =
        if isReverse then firstElemGreater else not firstElemGreater
      (prevElemGreater, wonkySt'') = greaterThan w x wonkySt'
      prevElemOrdered =
        if isReverse then prevElemGreater else not prevElemGreater
      rotateForward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
      rotateToLast =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
   in switch
        firstElemOrdered
        prevElemOrdered
        wonkySt'
        wonkySt''
        rotateForward
        rotateToLast
  where
    switch
      firstElemOrdered
      prevElemOrdered
      wonkySt'
      wonkySt''
      rotateForward
      rotateToLast
        | not isAmbi =
            rotationsortIterable
              greaterThan
              wonkySt
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not firstElemOrdered =
            rotationsortIterable
              greaterThan
              wonkySt'
              rotateToLast
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not prevElemOrdered =
            rotationsortIterable
              greaterThan
              wonkySt''
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThan
              wonkySt''
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
