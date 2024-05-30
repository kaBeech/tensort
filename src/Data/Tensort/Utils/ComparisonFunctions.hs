module Data.Tensort.Utils.ComparisonFunctions
  ( lessThanInt,
    lessThanRecord,
    greaterThanInt,
    greaterThanRecord,
    lessThanOrEqualInt,
    lessThanOrEqualRecord,
  )
where

import Data.Tensort.Utils.Types (Record)

lessThanInt :: Int -> Int -> Bool
lessThanInt x y = x < y

lessThanRecord :: Record -> Record -> Bool
lessThanRecord x y = snd x < snd y

greaterThanInt :: Int -> Int -> Bool
greaterThanInt x y = x > y

greaterThanRecord :: Record -> Record -> Bool
greaterThanRecord x y = snd x > snd y

lessThanOrEqualInt :: Int -> Int -> Bool
lessThanOrEqualInt x y = x <= y

lessThanOrEqualRecord :: Record -> Record -> Bool
lessThanOrEqualRecord x y = snd x <= snd y
