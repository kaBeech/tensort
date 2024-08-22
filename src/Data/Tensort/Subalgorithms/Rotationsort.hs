-- | This module provides Rotationsort variants for sorting lists using the
--   Sortable type
--
-- | I was having some issues with the swaps for larger input lists, so for now
--   these functions are only implemented for lists of length 3 or less.
module Data.Tensort.Subalgorithms.Rotationsort
  ( rotationsort,
    rotationsortAmbi,
    rotationsortReverse,
    rotationsortReverseAmbi,
  )
where

import Data.Tensort.Utils.ComparisonFunctions
  ( greaterThanOrEqualBit,
    greaterThanOrEqualRecord,
  )
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

-- | Takes a Sortable and returns a sorted Sortable using a Rotationsort
--  algorithm
--
--  I was having some issues with the swaps for larger input lists, so for now
--  this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsort (SortBit [1,3,2])
-- SortBit [1,2,3]
--
-- >>> rotationsort (SortRec [(3, 1), (1, 3), (2, 2)])
-- SortRec [(3,1),(2,2),(1,3)]
rotationsort :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsort wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanOrEqualBit wonkySt bits 0 False False
   in (SortBit result, wonkySt')
rotationsort wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanOrEqualRecord wonkySt recs 0 False False
   in (SortRec result, wonkySt')

rotationsortAmbi :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortAmbi wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanOrEqualBit wonkySt bits 0 True False
   in (SortBit result, wonkySt')
rotationsortAmbi wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanOrEqualRecord wonkySt recs 0 True False
   in (SortRec result, wonkySt')

rotationsortReverse :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortReverse wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanOrEqualBit
          wonkySt
          bits
          (length bits - 1)
          False
          True
   in (SortBit result, wonkySt')
rotationsortReverse wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanOrEqualRecord
          wonkySt
          recs
          (length recs - 1)
          False
          True
   in (SortRec result, wonkySt')

-- | Takes a Sortable and returns a sorted Sortable using an Ambidextrous
--   Reverse Rotationsort algorithm
--
--   I was having some issues with the swaps for larger input lists, so for now
--   this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsortReverseAmbi (SortBit [1,3,2])
-- SortBit [1,2,3]
--
-- >>> rotationsortReverseAmbi (SortRec [(3, 1), (1, 3), (2, 2)])
-- SortRec [(3,1),(2,2),(1,3)]
rotationsortReverseAmbi :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortReverseAmbi wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanOrEqualBit
          wonkySt
          bits
          (length bits - 1)
          True
          True
   in (SortBit result, wonkySt')
rotationsortReverseAmbi wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable
          greaterThanOrEqualRecord
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
rotationsortIterable greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse
  | length xs > 3 =
      error
        "From rotationsortIterable: algorithm not yet implemented for lists of length greater than 3"
  | currentIndex < 0 || currentIndex >= length xs =
      (xs, wonkySt)
  | length xs < 2 = (xs, wonkySt)
  | length xs == 2 =
      rotatationsortPair greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse
  | currentIndex == firstIndex (length xs) isReverse =
      rotationsortHead greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse
  | currentIndex == lastIndex (length xs) isReverse =
      rotationsortLast greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse
  | otherwise =
      rotationsortMiddle greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse

rotatationsortPair ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  ([a], WonkyState)
rotatationsortPair greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse =
  let x = head xs
      y = xs !! 1
      (secondElemGreater, wonkySt') = greaterThanOrEqual y x wonkySt
      swappedXs = y : [x]
   in switch secondElemGreater wonkySt' swappedXs
  where
    switch secondElemGreater wonkySt' swappedXs
      | not secondElemGreater =
          rotationsortIterable
            greaterThanOrEqual
            wonkySt'
            swappedXs
            (firstIndex (length xs) isReverse)
            isAmbi
            isReverse
      | otherwise =
          rotationsortIterable
            greaterThanOrEqual
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
rotationsortHead greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse =
  let w = xs !! lastIndex (length xs) isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      (lastElemOrdered', wonkySt') = lastElemOrdered greaterThanOrEqual wonkySt xs currentIndex isReverse
      (nextElemOrdered', wonkySt'') = nextElemOrdered greaterThanOrEqual wonkySt' xs currentIndex isReverse
      rotateToFirst =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
      rotateBackward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
   in switch
        lastElemOrdered'
        nextElemOrdered'
        wonkySt'
        wonkySt''
        rotateToFirst
        rotateBackward
  where
    switch
      lastElemOrdered'
      nextElemOrdered'
      wonkySt'
      wonkySt''
      rotateToFirst
      rotateBackward
        | not lastElemOrdered' =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt'
              rotateToFirst
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not nextElemOrdered' =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt''
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThanOrEqual
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
rotationsortMiddle greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      (nextElemOrdered', wonkySt') = nextElemOrdered greaterThanOrEqual wonkySt xs currentIndex isReverse
      (prevElemOrdered', wonkySt'') = prevElemOrdered greaterThanOrEqual wonkySt' xs currentIndex isReverse
      rotateBackward =
        if isReverse then [x] ++ [y] ++ [w] else [y] ++ [w] ++ [x]
      rotateForward =
        if isReverse then [y] ++ [w] ++ [x] else [x] ++ [y] ++ [w]
   in switch
        nextElemOrdered'
        prevElemOrdered'
        wonkySt'
        wonkySt''
        rotateBackward
        rotateForward
  where
    switch
      nextElemOrdered'
      prevElemOrdered'
      wonkySt'
      wonkySt''
      rotateBackward
      rotateForward
        | not nextElemOrdered' =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt'
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not isAmbi =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt'
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not prevElemOrdered' =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt''
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThanOrEqual
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
rotationsortLast greaterThanOrEqual wonkySt xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! firstIndex (length xs) isReverse
      (firstElemOrdered', wonkySt') = firstElemOrdered greaterThanOrEqual wonkySt xs currentIndex isReverse
      (prevElemOrdered', wonkySt'') = prevElemOrdered greaterThanOrEqual wonkySt' xs currentIndex isReverse
      rotateForward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
      rotateToLast =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
   in switch
        firstElemOrdered'
        prevElemOrdered'
        wonkySt'
        wonkySt''
        rotateForward
        rotateToLast
  where
    switch
      firstElemOrdered'
      prevElemOrdered'
      wonkySt'
      wonkySt''
      rotateForward
      rotateToLast
        | not isAmbi =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not firstElemOrdered' =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt'
              rotateToLast
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not prevElemOrdered' =
            rotationsortIterable
              greaterThanOrEqual
              wonkySt''
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThanOrEqual
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

nextElemOrdered ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  (Bool, WonkyState)
nextElemOrdered greaterThanOrEqual wonkySt xs currentIndex isReverse =
  let x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
   in if isReverse then greaterThanOrEqual x y wonkySt else greaterThanOrEqual y x wonkySt

prevElemOrdered ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  (Bool, WonkyState)
prevElemOrdered greaterThanOrEqual wonkySt xs currentIndex isReverse =
  let x = xs !! currentIndex
      w = xs !! prevIndex currentIndex isReverse
   in if isReverse then greaterThanOrEqual w x wonkySt else greaterThanOrEqual x w wonkySt

firstElemOrdered ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  (Bool, WonkyState)
firstElemOrdered greaterThanOrEqual wonkySt xs currentIndex isReverse =
  let x = xs !! currentIndex
      w = xs !! firstIndex (length xs) isReverse
   in if isReverse then greaterThanOrEqual w x wonkySt else greaterThanOrEqual x w wonkySt

lastElemOrdered ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  Bool ->
  (Bool, WonkyState)
lastElemOrdered greaterThanOrEqual wonkySt xs currentIndex isReverse =
  let x = xs !! currentIndex
      y = xs !! lastIndex (length xs) isReverse
   in if isReverse then greaterThanOrEqual x y wonkySt else greaterThanOrEqual y x wonkySt
