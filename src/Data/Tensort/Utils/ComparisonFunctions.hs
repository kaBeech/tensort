module Data.Tensort.Utils.ComparisonFunctions
  ( lessThanBit,
    lessThanRecord,
    greaterThanBit,
    greaterThanRecord,
    lessThanOrEqualBit,
    lessThanOrEqualRecord,
  )
where

import Data.Tensort.Utils.Types (Record, Bit)

lessThanBit :: Bit -> Bit -> Bool
lessThanBit x y = x < y

lessThanRecord :: Record -> Record -> Bool
lessThanRecord x y = snd x < snd y

greaterThanBit :: Bit -> Bit -> Bool
greaterThanBit x y = x > y

greaterThanRecord :: Record -> Record -> Bool
greaterThanRecord x y = snd x > snd y

lessThanOrEqualBit :: Bit -> Bit -> Bool
lessThanOrEqualBit x y = x <= y

lessThanOrEqualRecord :: Record -> Record -> Bool
lessThanOrEqualRecord x y = snd x <= snd y
