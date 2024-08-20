module Benchmarking.Score (getTotalErrorsScore) where

import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Score (getTotalPositionalErrors)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState (..), fromSortBit)
import System.Random (mkStdGen)

getTotalErrorsScore ::
  Int ->
  ( WonkyState ->
    Sortable ->
    (Sortable, WonkyState)
  ) ->
  Int ->
  Int ->
  Int ->
  Int
getTotalErrorsScore i sortAlg listLength wChance sChance = foldr acc 0 [1 .. i]
  where
    acc x totalScore = do
      let l = randomizeList x (SortBit [1 .. listLength])
      let wonkySt =
            WonkyState
              { wonkyChance = wChance,
                stuckChance = sChance,
                previousAnswer = 0,
                stdGen = mkStdGen x
              }
      let result = fst (sortAlg wonkySt l)
      let roundScore = getTotalPositionalErrors (fromSortBit result)
      totalScore + roundScore
