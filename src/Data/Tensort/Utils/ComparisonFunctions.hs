module Data.Tensort.Utils.ComparisonFunctions
  ( lessThanBit,
    lessThanRecord,
    greaterThanBit,
    greaterThanRecord,
    lessThanOrEqualBit,
    lessThanOrEqualRecord,
  )
where

import Data.Tensort.Utils.Types (Record)

lessThanBit :: Int -> Int -> Bool
lessThanBit x y = x < y

lessThanRecord :: Record -> Record -> Bool
lessThanRecord x y = snd x < snd y

greaterThanBit :: Int -> Int -> Bool
greaterThanBit x y = x > y

greaterThanRecord :: Record -> Record -> Bool
greaterThanRecord x y = snd x > snd y

lessThanOrEqualBit :: Int -> Int -> Bool
lessThanOrEqualBit x y = x <= y

lessThanOrEqualRecord :: Record -> Record -> Bool
lessThanOrEqualRecord x y = snd x <= snd y
