module Data.Tensort.Subalgorithms.Exchangesort (exchangesort) where

import Data.Tensort.Utils.ComparisonFunctions (greaterThanBit, greaterThanRecord)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState)

exchangesort :: Sortable -> WonkyState -> (Sortable, WonkyState)
exchangesort (SortBit bits) wonkySt = do
  let (result, wonkySt') = exchangesortIterable bits (length bits - 1) (length bits - 2) greaterThanBit wonkySt
  (SortBit result, wonkySt')
exchangesort (SortRec recs) wonkySt = do
  let (result, wonkySt') = exchangesortIterable recs (length recs - 1) (length recs - 2) greaterThanRecord wonkySt
  (SortRec result, wonkySt')

exchangesortIterable :: [a] -> Int -> Int -> (a -> a -> WonkyState -> (Bool, WonkyState)) -> WonkyState -> ([a], WonkyState)
exchangesortIterable xs i j greaterThan wonkySt = do
  if i < 0
    then (xs, wonkySt)
    else
      if j < 0
        then exchangesortIterable xs (i - 1) (length xs - 1) greaterThan wonkySt
        else
          if i > j
            then do
              let (firstElemGreater, wonkySt') = greaterThan (xs !! j) (xs !! i) wonkySt
              if firstElemGreater
                then exchangesortIterable (swap xs i j) i (j - 1) greaterThan wonkySt'
                else exchangesortIterable xs i (j - 1) greaterThan wonkySt
            else
              if j > i
                then do
                  let (firstElemGreater, wonkySt') = greaterThan (xs !! i) (xs !! j) wonkySt
                  if firstElemGreater
                    then exchangesortIterable (swap xs i j) i (j - 1) greaterThan wonkySt'
                    else exchangesortIterable xs i (j - 1) greaterThan wonkySt'
                else exchangesortIterable xs i (j - 1) greaterThan wonkySt

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
