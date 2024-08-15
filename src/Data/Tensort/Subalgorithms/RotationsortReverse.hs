module Data.Tensort.Subalgorithms.RotationsortReverse
  ( rotationsortReverse,
    rotationsortAmbiReverse,
  )
where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

rotationsortReverse :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortReverse wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortReverseIterable greaterThanBit wonkySt bits (length bits)
   in (SortBit result, wonkySt')
rotationsortReverse wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortReverseIterable greaterThanRecord wonkySt recs (length recs)
   in (SortRec result, wonkySt')

rotationsortReverseIterable ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  ([a], WonkyState)
rotationsortReverseIterable greaterThan wonkySt xs currentIndex
  | length xs < 2 =
      (xs, wonkySt)
  | length xs == 2 =
      let x = head xs
          y = xs !! 1
          (secondElemGreater, wonkySt') = greaterThan y x wonkySt
          swappedXs = y : [x]
       in if not secondElemGreater
            then rotationsortReverseIterable greaterThan wonkySt' swappedXs 1
            else
              if currentIndex < 1
                then (xs, wonkySt')
                else rotationsortReverseIterable greaterThan wonkySt' xs 0
  | currentIndex >= 2 =
      let w = xs !! 1
          x = xs !! 2
          y = xs !! 0
          (firstElemGreater, wonkySt') = greaterThan y x wonkySt
          (leftElemGreater, wonkySt'') = greaterThan w x wonkySt'
          rotateForward = [w] ++ [x] ++ [y]
          rotateRight = [y] ++ [x] ++ [w]
       in if firstElemGreater
            then rotationsortReverseIterable greaterThan wonkySt' rotateForward 2
            else
              if leftElemGreater
                then rotationsortReverseIterable greaterThan wonkySt'' rotateRight 2
                else rotationsortReverseIterable greaterThan wonkySt'' xs (currentIndex - 1)
  | currentIndex < 1 = (xs, wonkySt)
  | otherwise =
      let left = take (currentIndex - 1) xs
          right = drop (currentIndex + 2) xs
          w = xs !! 0
          x = xs !! 1
          y = xs !! 2
          (rightElemGreater, wonkySt') = greaterThan y x wonkySt
          (leftElemGreater, wonkySt'') = greaterThan w x wonkySt'
          rotateLeft = [y] ++ [w] ++ [x]
          rotateRight = [x] ++ [y] ++ [w]
       in if leftElemGreater
            then rotationsortReverseIterable greaterThan wonkySt'' rotateRight 2
            else rotationsortReverseIterable greaterThan wonkySt' xs (currentIndex - 1)

rotationsortAmbiReverse :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortAmbiReverse wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortReverseAmbiIterable greaterThanBit wonkySt bits (length bits)
   in (SortBit result, wonkySt')
rotationsortAmbiReverse wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortReverseAmbiIterable greaterThanRecord wonkySt recs (length recs)
   in (SortRec result, wonkySt')

-- Ambidextrious reverse Rotationsort
rotationsortReverseAmbiIterable ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  ([a], WonkyState)
rotationsortReverseAmbiIterable greaterThan wonkySt xs currentIndex
  | length xs < 2 =
      (xs, wonkySt)
  | length xs == 2 =
      let x = head xs
          y = xs !! 1
          (secondElemGreater, wonkySt') = greaterThan y x wonkySt
          swappedXs = y : [x]
       in if not secondElemGreater
            then rotationsortReverseAmbiIterable greaterThan wonkySt' swappedXs 1
            else
              if currentIndex < 1
                then (xs, wonkySt')
                else rotationsortReverseAmbiIterable greaterThan wonkySt' xs 0
  | currentIndex >= 2 =
      let w = xs !! 1
          x = xs !! 2
          y = xs !! 0
          (firstElemGreater, wonkySt') = greaterThan y x wonkySt
          (leftElemGreater, wonkySt'') = greaterThan w x wonkySt'
          rotateForward = [w] ++ [x] ++ [y]
          rotateRight = [y] ++ [x] ++ [w]
       in if firstElemGreater
            then rotationsortReverseAmbiIterable greaterThan wonkySt' rotateForward 2
            else
              if leftElemGreater
                then rotationsortReverseAmbiIterable greaterThan wonkySt'' rotateRight 2
                else rotationsortReverseAmbiIterable greaterThan wonkySt'' xs (currentIndex - 1)
  | currentIndex < 1 =
      let w = xs !! 2
          x = xs !! 0
          y = xs !! 1
          (lastElemGreater, wonkySt') = greaterThan w x wonkySt
          (rightElemGreater, wonkySt'') = greaterThan y x wonkySt'
          rotateLeft = [y] ++ [x] ++ [w]
          rotateBackward = [w] ++ [x] ++ [y]
       in if not lastElemGreater
            then rotationsortReverseAmbiIterable greaterThan wonkySt' rotateBackward (currentIndex + 1)
            else
              if not rightElemGreater
                then rotationsortReverseAmbiIterable greaterThan wonkySt'' rotateLeft (currentIndex + 1)
                else (xs, wonkySt'')
  | otherwise =
      let left = take (currentIndex - 1) xs
          right = drop (currentIndex + 2) xs
          w = xs !! 0
          x = xs !! 1
          y = xs !! 2
          (rightElemGreater, wonkySt') = greaterThan y x wonkySt
          (leftElemGreater, wonkySt'') = greaterThan w x wonkySt'
          rotateLeft = [y] ++ [w] ++ [x]
          rotateRight = [x] ++ [y] ++ [w]
       in if leftElemGreater
            then rotationsortReverseAmbiIterable greaterThan wonkySt'' rotateRight 2
            else
              if not rightElemGreater
                then rotationsortReverseAmbiIterable greaterThan wonkySt' rotateLeft (currentIndex + 1)
                else rotationsortReverseAmbiIterable greaterThan wonkySt' xs (currentIndex - 1)
