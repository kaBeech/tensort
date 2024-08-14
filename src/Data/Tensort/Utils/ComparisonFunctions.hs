module Data.Tensort.Utils.ComparisonFunctions
  ( lessThanBit,
    lessThanRecord,
    greaterThanBit,
    greaterThanRecord,
    lessThanOrEqualBit,
    lessThanOrEqualRecord,
    equalBit,
    equalRecord,
  )
where

import Data.Tensort.Utils.Types (Bit, Record, WonkyState)
import Data.Tensort.Utils.Wonky (handleWonkiness, setPreviousAnswer)

wonkyCompare :: Bit -> Bit -> WonkyState -> (Int, WonkyState)
wonkyCompare x y wonkySt = do
  let (wonky, result, wonkySt') = handleWonkiness wonkySt
  if wonky
    then (result, setPreviousAnswer wonkySt' result)
    else
      if x < y
        then (-1, setPreviousAnswer wonkySt' (-1))
        else
          if x > y
            then (1, setPreviousAnswer wonkySt' 1)
            else (0, setPreviousAnswer wonkySt' 0)

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

equalBit :: Bit -> Bit -> WonkyState -> (Bool, WonkyState)
equalBit x y wonkySt = do
  let (result, wonkySt') = wonkyCompare x y wonkySt
  (result == 0, wonkySt')

equalRecord :: Record -> Record -> WonkyState -> (Bool, WonkyState)
equalRecord x y wonkySt = do
  let (result, wonkySt') = wonkyCompare (snd x) (snd y) wonkySt
  (result == 0, wonkySt')
