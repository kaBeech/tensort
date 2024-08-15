module Data.Tensort.Subalgorithms.Rotationsort
  ( rotationsortAmbi,
    rotationsort,
  )
where

import Data.Tensort.Utils.ComparisonFunctions
  ( greaterThanBit,
    greaterThanRecord,
  )
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

rotationsort :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsort wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanBit wonkySt bits 0
   in (SortBit result, wonkySt')
rotationsort wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortIterable greaterThanRecord wonkySt recs 0
   in (SortRec result, wonkySt')

rotationsortIterable ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  ([a], WonkyState)
rotationsortIterable greaterThan wonkySt xs currentIndex
  | length xs < 2 =
      (xs, wonkySt)
  | length xs == 2 =
      let x = head xs
          y = xs !! 1
          (secondElemGreater, wonkySt') = greaterThan y x wonkySt
          swappedXs = y : [x]
       in if not secondElemGreater
            then rotationsortIterable greaterThan wonkySt' swappedXs 0
            else
              if currentIndex > 0
                then (xs, wonkySt')
                else rotationsortIterable greaterThan wonkySt' xs 1
  | currentIndex < 1 =
      let right = drop (currentIndex + 2) xs
          w = xs !! (length xs - 1)
          x = xs !! currentIndex
          y = xs !! (currentIndex + 1)
          (lastElemGreater, wonkySt') = greaterThan w x wonkySt
          (rightElemGreater, wonkySt'') = greaterThan y x wonkySt'
          rotateBackward = [w] ++ [x] ++ [y]
          rotateLeft = [y] ++ [x] ++ [w]
       in if not lastElemGreater
            then rotationsortIterable greaterThan wonkySt' rotateBackward 0
            else
              if not rightElemGreater
                then rotationsortIterable greaterThan wonkySt'' rotateLeft 0
                else rotationsortIterable greaterThan wonkySt'' xs (currentIndex + 1)
  | currentIndex >= 2 = (xs, wonkySt)
  | otherwise =
      let left = take (currentIndex - 1) xs
          right = drop (currentIndex + 2) xs
          w = xs !! (currentIndex - 1)
          x = xs !! currentIndex
          y = xs !! (currentIndex + 1)
          (rightElemGreater, wonkySt') = greaterThan y x wonkySt
          (leftElemGreater, wonkySt'') = greaterThan w x wonkySt'
          rotateLeft = [y] ++ [w] ++ [x]
          rotateRight = [x] ++ [y] ++ [w]
       in if not rightElemGreater
            then rotationsortIterable greaterThan wonkySt' rotateLeft 0
            else
              if leftElemGreater
                then rotationsortIterable greaterThan wonkySt'' rotateRight 0
                else rotationsortIterable greaterThan wonkySt'' xs (currentIndex + 1)

rotationsortAmbi :: WonkyState -> Sortable -> (Sortable, WonkyState)
rotationsortAmbi wonkySt (SortBit bits) =
  let (result, wonkySt') =
        rotationsortAmbiIterable greaterThanBit wonkySt bits 0
   in (SortBit result, wonkySt')
rotationsortAmbi wonkySt (SortRec recs) =
  let (result, wonkySt') =
        rotationsortAmbiIterable greaterThanRecord wonkySt recs 0
   in (SortRec result, wonkySt')

rotationsortAmbiIterable ::
  (Ord a) =>
  (a -> a -> WonkyState -> (Bool, WonkyState)) ->
  WonkyState ->
  [a] ->
  Int ->
  ([a], WonkyState)
rotationsortAmbiIterable greaterThan wonkySt xs currentIndex
  | length xs < 2 =
      (xs, wonkySt)
  | length xs == 2 =
      let x = head xs
          y = xs !! 1
          (secondElemGreater, wonkySt') = greaterThan y x wonkySt
          swappedXs = y : [x]
       in if not secondElemGreater
            then rotationsortAmbiIterable greaterThan wonkySt' swappedXs 0
            else
              if currentIndex > 0
                then (xs, wonkySt')
                else rotationsortAmbiIterable greaterThan wonkySt' xs 1
  | currentIndex < 1 =
      let right = drop (currentIndex + 2) xs
          w = xs !! 2
          x = xs !! 0
          y = xs !! 1
          (lastElemGreater, wonkySt') = greaterThan w x wonkySt
          (rightElemGreater, wonkySt'') = greaterThan y x wonkySt'
          rotateBackward = [w] ++ [x] ++ [y]
          rotateLeft = [y] ++ [x] ++ [w]
       in if not lastElemGreater
            then rotationsortAmbiIterable greaterThan wonkySt' rotateBackward 0
            else
              if not rightElemGreater
                then rotationsortAmbiIterable greaterThan wonkySt'' rotateLeft 0
                else rotationsortAmbiIterable greaterThan wonkySt'' xs (currentIndex + 1)
  | currentIndex >= 2 =
      let left = take (currentIndex - 1) xs
          w = xs !! 1
          x = xs !! 2
          y = xs !! 0
          (firstElemGreater, wonkySt') = greaterThan y x wonkySt
          (leftElemGreater, wonkySt'') = greaterThan w x wonkySt'
          rotateRight = [y] ++ [x] ++ [w]
          rotateForward = [w] ++ [x] ++ [y]
       in if firstElemGreater
            then rotationsortAmbiIterable greaterThan wonkySt' rotateForward (currentIndex - 1)
            else
              if leftElemGreater
                then rotationsortAmbiIterable greaterThan wonkySt'' rotateRight (currentIndex - 1)
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
       in if not rightElemGreater
            then rotationsortAmbiIterable greaterThan wonkySt' rotateLeft 0
            else
              if leftElemGreater
                then rotationsortAmbiIterable greaterThan wonkySt'' rotateRight (currentIndex - 1)
                else rotationsortAmbiIterable greaterThan wonkySt'' xs (currentIndex + 1)
