module Data.Tensort.Utils.ComparisonFunctions
  ( lessThanBit,
    lessThanRecord,
    lessThanOrEqualBit,
    lessThanOrEqualRecord,
    greaterThanBit,
    greaterThanRecord,
    greaterThanOrEqualBit,
    greaterThanOrEqualRecord,
    equalBit,
    equalRecord,
  )
where

import Data.Tensort.Utils.Types (Bit, Record)

lessThanBit :: Bit -> Bit -> Bool
lessThanBit x y = x < y

lessThanRecord :: Record -> Record -> Bool
lessThanRecord x y = snd x < snd y

lessThanOrEqualBit :: Bit -> Bit -> Bool
lessThanOrEqualBit x y = x <= y

lessThanOrEqualRecord :: Record -> Record -> Bool
lessThanOrEqualRecord x y = snd x <= snd y

greaterThanBit :: Bit -> Bit -> Bool
greaterThanBit x y = x > y

greaterThanRecord :: Record -> Record -> Bool
greaterThanRecord x y = snd x > snd y

greaterThanOrEqualBit :: Bit -> Bit -> Bool
greaterThanOrEqualBit x y = x >= y

greaterThanOrEqualRecord :: Record -> Record -> Bool
greaterThanOrEqualRecord x y = snd x >= snd y

equalBit :: Bit -> Bit -> Bool
equalBit x y = x == y

equalRecord :: Record -> Record -> Bool
equalRecord x y = snd x == snd y
