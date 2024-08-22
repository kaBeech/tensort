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
import Data.Tensort.Utils.Types (Sortable (..))

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
rotationsort :: Sortable -> Sortable
rotationsort (SortBit bits) =
  let result =
        rotationsortIterable greaterThanOrEqualBit bits 0 False False
   in SortBit result
rotationsort (SortRec recs) =
  let result =
        rotationsortIterable greaterThanOrEqualRecord recs 0 False False
   in SortRec result

-- | Takes a Sortable and returns a sorted Sortable using an Ambidextrous
--   Rotationsort algorithm
--
--  I was having some issues with the swaps for larger input lists, so for now
--  this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsortAmbi (SortBit [1,3,2])
-- SortBit [1,2,3]
--
-- >>> rotationsortAmbi (SortRec [(3, 1), (1, 3), (2, 2)])
-- SortRec [(3,1),(2,2),(1,3)]
rotationsortAmbi :: Sortable -> Sortable
rotationsortAmbi (SortBit bits) =
  let result =
        rotationsortIterable greaterThanOrEqualBit bits 0 True False
   in SortBit result
rotationsortAmbi (SortRec recs) =
  let result =
        rotationsortIterable greaterThanOrEqualRecord recs 0 True False
   in SortRec result

-- | Takes a Sortable and returns a sorted Sortable using a Reverse
--   Rotationsort algorithm
--
--   I was having some issues with the swaps for larger input lists, so for now
--   this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsortReverse (SortBit [1,3,2])
-- SortBit [1,2,3]
--
-- >>> rotationsortReverse (SortRec [(3, 1), (1, 3), (2, 2)])
-- SortRec [(3,1),(2,2),(1,3)]
rotationsortReverse :: Sortable -> Sortable
rotationsortReverse (SortBit bits) =
  let result =
        rotationsortIterable
          greaterThanOrEqualBit
          bits
          (length bits - 1)
          False
          True
   in SortBit result
rotationsortReverse (SortRec recs) =
  let result =
        rotationsortIterable
          greaterThanOrEqualRecord
          recs
          (length recs - 1)
          False
          True
   in SortRec result

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
rotationsortReverseAmbi :: Sortable -> Sortable
rotationsortReverseAmbi (SortBit bits) =
  let result =
        rotationsortIterable
          greaterThanOrEqualBit
          bits
          (length bits - 1)
          True
          True
   in SortBit result
rotationsortReverseAmbi (SortRec recs) =
  let result =
        rotationsortIterable
          greaterThanOrEqualRecord
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
rotationsortIterable greaterThanOrEqual xs currentIndex isAmbi isReverse
  | length xs > 3 =
      error
        "From rotationsortIterable: algorithm not yet implemented for lists of length greater than 3"
  | currentIndex < 0 || currentIndex >= length xs =
      xs
  | length xs < 2 = xs
  | length xs == 2 =
      rotatationsortPair greaterThanOrEqual xs currentIndex isAmbi isReverse
  | currentIndex == firstIndex (length xs) isReverse =
      rotationsortHead greaterThanOrEqual xs currentIndex isAmbi isReverse
  | currentIndex == lastIndex (length xs) isReverse =
      rotationsortLast greaterThanOrEqual xs currentIndex isAmbi isReverse
  | otherwise =
      rotationsortMiddle greaterThanOrEqual xs currentIndex isAmbi isReverse

rotatationsortPair ::
  (Ord a) =>
  (a -> a -> Bool) ->
  [a] ->
  Int ->
  Bool ->
  Bool ->
  [a]
rotatationsortPair greaterThanOrEqual xs currentIndex isAmbi isReverse =
  let x = head xs
      y = xs !! 1
      secondElemGreater = greaterThanOrEqual y x
      swappedXs = y : [x]
   in switch secondElemGreater swappedXs
  where
    switch secondElemGreater swappedXs
      | not secondElemGreater =
          rotationsortIterable
            greaterThanOrEqual
            swappedXs
            (firstIndex (length xs) isReverse)
            isAmbi
            isReverse
      | otherwise =
          rotationsortIterable
            greaterThanOrEqual
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
rotationsortHead greaterThanOrEqual xs currentIndex isAmbi isReverse =
  let w = xs !! lastIndex (length xs) isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      rotateToFirst =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
      rotateBackward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
   in switch
        rotateToFirst
        rotateBackward
  where
    switch
      rotateToFirst
      rotateBackward
        | not (lastElemOrdered greaterThanOrEqual xs currentIndex isReverse) =
            rotationsortIterable
              greaterThanOrEqual
              rotateToFirst
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not (nextElemOrdered greaterThanOrEqual xs currentIndex isReverse) =
            rotationsortIterable
              greaterThanOrEqual
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThanOrEqual
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
rotationsortMiddle greaterThanOrEqual xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
      rotateBackward =
        if isReverse then [x] ++ [y] ++ [w] else [y] ++ [w] ++ [x]
      rotateForward =
        if isReverse then [y] ++ [w] ++ [x] else [x] ++ [y] ++ [w]
   in switch
        rotateBackward
        rotateForward
  where
    switch
      rotateBackward
      rotateForward
        | not (nextElemOrdered greaterThanOrEqual xs currentIndex isReverse) =
            rotationsortIterable
              greaterThanOrEqual
              rotateBackward
              (firstIndex (length xs) isReverse)
              isAmbi
              isReverse
        | not isAmbi =
            rotationsortIterable
              greaterThanOrEqual
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not (prevElemOrdered greaterThanOrEqual xs currentIndex isReverse) =
            rotationsortIterable
              greaterThanOrEqual
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThanOrEqual
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
rotationsortLast greaterThanOrEqual xs currentIndex isAmbi isReverse =
  let w = xs !! prevIndex currentIndex isReverse
      x = xs !! currentIndex
      y = xs !! firstIndex (length xs) isReverse
      rotateForward =
        if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
      rotateToLast =
        if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
   in switch
        rotateForward
        rotateToLast
  where
    switch
      rotateForward
      rotateToLast
        | not isAmbi =
            rotationsortIterable
              greaterThanOrEqual
              xs
              (nextIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not (firstElemOrdered greaterThanOrEqual xs currentIndex isReverse) =
            rotationsortIterable
              greaterThanOrEqual
              rotateToLast
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | not (prevElemOrdered greaterThanOrEqual xs currentIndex isReverse) =
            rotationsortIterable
              greaterThanOrEqual
              rotateForward
              (prevIndex currentIndex isReverse)
              isAmbi
              isReverse
        | otherwise =
            rotationsortIterable
              greaterThanOrEqual
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

nextElemOrdered :: (Ord a) => (a -> a -> Bool) -> [a] -> Int -> Bool -> Bool
nextElemOrdered greaterThanOrEqual xs currentIndex isReverse =
  let x = xs !! currentIndex
      y = xs !! nextIndex currentIndex isReverse
   in if isReverse then greaterThanOrEqual x y else greaterThanOrEqual y x

prevElemOrdered :: (Ord a) => (a -> a -> Bool) -> [a] -> Int -> Bool -> Bool
prevElemOrdered greaterThanOrEqual xs currentIndex isReverse =
  let x = xs !! currentIndex
      w = xs !! prevIndex currentIndex isReverse
   in if isReverse then greaterThanOrEqual w x else greaterThanOrEqual x w

firstElemOrdered :: (Ord a) => (a -> a -> Bool) -> [a] -> Int -> Bool -> Bool
firstElemOrdered greaterThanOrEqual xs currentIndex isReverse =
  let x = xs !! currentIndex
      w = xs !! firstIndex (length xs) isReverse
   in if isReverse then greaterThanOrEqual w x else greaterThanOrEqual x w

lastElemOrdered :: (Ord a) => (a -> a -> Bool) -> [a] -> Int -> Bool -> Bool
lastElemOrdered greaterThanOrEqual xs currentIndex isReverse =
  let x = xs !! currentIndex
      y = xs !! lastIndex (length xs) isReverse
   in if isReverse then greaterThanOrEqual x y else greaterThanOrEqual y x
