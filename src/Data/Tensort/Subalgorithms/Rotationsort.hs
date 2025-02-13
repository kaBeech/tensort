-- | This module provides Rotationsort variants for sorting lists.
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

-- | Takes a list and returns a sorted list using a Rotationsort
--  algorithm.
--
--  I was having some issues with the swaps for larger input lists, so for now
--  this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsort [1,3,2]
-- [1,2,3]
--
-- >>> rotationsort [(3, 1), (1, 3), (2, 2)]
-- [(3,1),(2,2),(1,3)]
rotationsort :: (Ord a) => [a] -> [a]
rotationsort bits = rotationsort' bits 0 False False

-- | Takes a list and returns a sorted list using an Ambidextrous
--   Rotationsort algorithm.
--
--  I was having some issues with the swaps for larger input lists, so for now
--  this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsortAmbi [1,3,2]
-- [1,2,3]
--
-- >>> rotationsortAmbi [(3, 1), (1, 3), (2, 2)]
-- [(3,1),(2,2),(1,3)]
rotationsortAmbi :: (Ord a) => [a] -> [a]
rotationsortAmbi bits = rotationsort' bits 0 True False

-- | Takes a list and returns a sorted list using a Reverse
--   Rotationsort algorithm.
--
--   I was having some issues with the swaps for larger input lists, so for now
--   this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsortReverse [1,3,2]
-- [1,2,3]
--
-- >>> rotationsortReverse [(3, 1), (1, 3), (2, 2)]
-- [(3,1),(2,2),(1,3)]
rotationsortReverse :: (Ord a) => [a] -> [a]
rotationsortReverse bits = rotationsort' bits (length bits - 1) False True

-- | Takes a list and returns a sorted list using an Ambidextrous
--   Reverse Rotationsort algorithm.
--
--   I was having some issues with the swaps for larger input lists, so for now
--   this function is only implemented for lists of length 3 or less.

-- | ==== __Examples__
-- >>> rotationsortReverseAmbi [1,3,2]
-- [1,2,3]
--
-- >>> rotationsortReverseAmbi [(3, 1), (1, 3), (2, 2)]
-- [(3,1),(2,2),(1,3)]
rotationsortReverseAmbi :: (Ord a) => [a] -> [a]
rotationsortReverseAmbi bits = rotationsort' bits (length bits - 1) True True

rotationsort' :: (Ord a) => [a] -> Int -> Bool -> Bool -> [a]
rotationsort' xs currentIndex isAmbi isReverse
  | length xs > 3 =
      error
        "From rotationsort': algorithm not yet implemented for lists of length greater than 3"
  | currentIndex < 0 || currentIndex >= length xs =
      xs
  | length xs < 2 = xs
  | length xs == 2 =
      rotatationsortPair xs currentIndex isAmbi isReverse
  | currentIndex == firstIndex (length xs) isReverse =
      rotationsortHead xs currentIndex isAmbi isReverse
  | currentIndex == lastIndex (length xs) isReverse =
      rotationsortLast xs currentIndex isAmbi isReverse
  | otherwise =
      rotationsortMiddle xs currentIndex isAmbi isReverse

rotatationsortPair :: (Ord a) => [a] -> Int -> Bool -> Bool -> [a]
rotatationsortPair xs currentIndex isAmbi isReverse
  | not secondElemGreater =
      rotationsort'
        swappedXs
        (firstIndex (length xs) isReverse)
        isAmbi
        isReverse
  | otherwise =
      rotationsort'
        xs
        (nextIndex currentIndex isReverse)
        isAmbi
        isReverse
  where
    x = head xs
    y = xs !! 1
    secondElemGreater = y >= x
    swappedXs = y : [x]

rotationsortHead :: (Ord a) => [a] -> Int -> Bool -> Bool -> [a]
rotationsortHead xs currentIndex isAmbi isReverse
  | not $ lastElemOrdered xs currentIndex isReverse =
      rotationsort'
        rotateToFirst
        (firstIndex (length xs) isReverse)
        isAmbi
        isReverse
  | not $ nextElemOrdered xs currentIndex isReverse =
      rotationsort'
        rotateBackward
        (firstIndex (length xs) isReverse)
        isAmbi
        isReverse
  | otherwise =
      rotationsort'
        xs
        (nextIndex currentIndex isReverse)
        isAmbi
        isReverse
  where
    w = xs !! lastIndex (length xs) isReverse
    x = xs !! currentIndex
    y = xs !! nextIndex currentIndex isReverse
    rotateToFirst =
      if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]
    rotateBackward =
      if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]

rotationsortMiddle :: (Ord a) => [a] -> Int -> Bool -> Bool -> [a]
rotationsortMiddle xs currentIndex isAmbi isReverse
  | not $ nextElemOrdered xs currentIndex isReverse =
      rotationsort'
        rotateBackward
        (firstIndex (length xs) isReverse)
        isAmbi
        isReverse
  | not isAmbi =
      rotationsort'
        xs
        (nextIndex currentIndex isReverse)
        isAmbi
        isReverse
  | not $ prevElemOrdered xs currentIndex isReverse =
      rotationsort'
        rotateForward
        (prevIndex currentIndex isReverse)
        isAmbi
        isReverse
  | otherwise =
      rotationsort'
        xs
        (nextIndex currentIndex isReverse)
        isAmbi
        isReverse
  where
    w = xs !! prevIndex currentIndex isReverse
    x = xs !! currentIndex
    y = xs !! nextIndex currentIndex isReverse
    rotateBackward =
      if isReverse then [x] ++ [y] ++ [w] else [y] ++ [w] ++ [x]
    rotateForward =
      if isReverse then [y] ++ [w] ++ [x] else [x] ++ [y] ++ [w]

rotationsortLast ::
  (Ord a) => [a] -> Int -> Bool -> Bool -> [a]
rotationsortLast xs currentIndex isAmbi isReverse
  | not isAmbi =
      rotationsort'
        xs
        (nextIndex currentIndex isReverse)
        isAmbi
        isReverse
  | not $ firstElemOrdered xs currentIndex isReverse =
      rotationsort'
        rotateToLast
        (prevIndex currentIndex isReverse)
        isAmbi
        isReverse
  | not $ prevElemOrdered xs currentIndex isReverse =
      rotationsort'
        rotateForward
        (prevIndex currentIndex isReverse)
        isAmbi
        isReverse
  | otherwise =
      rotationsort'
        xs
        (nextIndex currentIndex isReverse)
        isAmbi
        isReverse
  where
    w = xs !! prevIndex currentIndex isReverse
    x = xs !! currentIndex
    y = xs !! firstIndex (length xs) isReverse
    rotateForward =
      if isReverse then [w] ++ [x] ++ [y] else [y] ++ [x] ++ [w]
    rotateToLast =
      if isReverse then [y] ++ [x] ++ [w] else [w] ++ [x] ++ [y]

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

nextElemOrdered :: (Ord a) => [a] -> Int -> Bool -> Bool
nextElemOrdered xs currentIndex isReverse = if isReverse then x >= y else y >= x
  where
    x = xs !! currentIndex
    y = xs !! nextIndex currentIndex isReverse

prevElemOrdered :: (Ord a) => [a] -> Int -> Bool -> Bool
prevElemOrdered xs currentIndex isReverse = if isReverse then w >= x else x >= w
  where
    x = xs !! currentIndex
    w = xs !! prevIndex currentIndex isReverse

firstElemOrdered :: (Ord a) => [a] -> Int -> Bool -> Bool
firstElemOrdered xs currentIndex isReverse = if isReverse then w >= x else x >= w
  where
    x = xs !! currentIndex
    w = xs !! firstIndex (length xs) isReverse

lastElemOrdered :: (Ord a) => [a] -> Int -> Bool -> Bool
lastElemOrdered xs currentIndex isReverse = if isReverse then x >= y else y >= x
  where
    x = xs !! currentIndex
    y = xs !! lastIndex (length xs) isReverse
