module Data.Tensort.Utils.ComparisonFunctions
  ( lessThanBit,
    lessThanRecord,
    greaterThanBit,
    greaterThanRecord,
    lessThanOrEqualBit,
    lessThanOrEqualRecord,
  )
where

import Data.Tensort.Utils.Types (Bit, Record, WonkyState)
import Data.Tensort.Utils.Wonky (handleWonkiness)

wonkyCompare :: Bit -> Bit -> WonkyState -> (Int, WonkyState)
wonkyCompare x y wonkySt = do
  let (wonky, result, wonkySt') = handleWonkiness wonkySt
  if wonky
    then (result, wonkySt')
    else
      if x < y
        then (-1, wonkySt')
        else
          if x > y
            then (1, wonkySt')
            else (0, wonkySt')

lessThanBit :: Bit -> Bit -> WonkyState -> (Bool, WonkyState)
lessThanBit x y wonkySt = do
  let (result, wonkySt') = wonkyCompare x y wonkySt
  (result == -1, wonkySt')

lessThanRecord :: Record -> Record -> WonkyState -> (Bool, WonkyState)
lessThanRecord x y wonkySt = do
  let (result, wonkySt') = wonkyCompare (snd x) (snd y) wonkySt
  (result == -1, wonkySt')

greaterThanBit :: Bit -> Bit -> WonkyState -> (Bool, WonkyState)
greaterThanBit x y wonkySt = do
  let (result, wonkySt') = wonkyCompare x y wonkySt
  (result == 1, wonkySt')

greaterThanRecord :: Record -> Record -> WonkyState -> (Bool, WonkyState)
greaterThanRecord x y wonkySt = do
  let (result, wonkySt') = wonkyCompare (snd x) (snd y) wonkySt
  (result == 1, wonkySt')

lessThanOrEqualBit :: Bit -> Bit -> WonkyState -> (Bool, WonkyState)
lessThanOrEqualBit x y wonkySt = do
  let (result, wonkySt') = wonkyCompare x y wonkySt
  (result <= 0, wonkySt')

lessThanOrEqualRecord :: Record -> Record -> WonkyState -> (Bool, WonkyState)
lessThanOrEqualRecord x y wonkySt = do
  let (result, wonkySt') = wonkyCompare (snd x) (snd y) wonkySt
  (result <= 0, wonkySt')
