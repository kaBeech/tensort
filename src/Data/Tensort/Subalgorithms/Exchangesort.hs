module Data.Tensort.Subalgorithms.Exchangesort (exchangesort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

exchangesort :: WonkyState -> Sortable -> (Sortable, WonkyState)
exchangesort wonkySt (SortBit bits) = do
  let (result, wonkySt') = exchangesortIterable greaterThanBit wonkySt bits (length bits - 1) (length bits - 2)
  (SortBit result, wonkySt')
exchangesort wonkySt (SortRec recs) = do
  let (result, wonkySt') = exchangesortIterable greaterThanRecord wonkySt recs (length recs - 1) (length recs - 2)
  (SortRec result, wonkySt')

exchangesortIterable :: (a -> a -> WonkyState -> (Bool, WonkyState)) -> WonkyState -> [a] -> Int -> Int -> ([a], WonkyState)
exchangesortIterable greaterThan wonkySt xs i j = do
  if i < 0
    then (xs, wonkySt)
    else
      if j < 0
        then exchangesortIterable greaterThan wonkySt xs (i - 1) (length xs - 1)
        else
          if i > j
            then do
              let (firstElemGreater, wonkySt') = greaterThan (xs !! j) (xs !! i) wonkySt
              if firstElemGreater
                then exchangesortIterable greaterThan wonkySt' (swap xs i j) i (j - 1)
                else exchangesortIterable greaterThan wonkySt xs i (j - 1)
            else
              if j > i
                then do
                  let (firstElemGreater, wonkySt') = greaterThan (xs !! i) (xs !! j) wonkySt
                  if firstElemGreater
                    then exchangesortIterable greaterThan wonkySt' (swap xs i j) i (j - 1)
                    else exchangesortIterable greaterThan wonkySt' xs i (j - 1)
                else exchangesortIterable greaterThan wonkySt xs i (j - 1)

swap :: [a] -> Int -> Int -> [a]
swap xs i j = do
  let x = xs !! i
  let y = xs !! j
  let mini = min i j
  let maxi = max i j
  let left = take mini xs
  let middle = take (maxi - mini - 1) (drop (mini + 1) xs)
  let right = drop (maxi + 1) xs
  left ++ [y] ++ middle ++ [x] ++ right
