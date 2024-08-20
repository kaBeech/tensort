module Benchmarking.Score (getTotalErrorsScore, getSingleRunErrorsScore) where

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
    acc seed totalScore =
      let currentRoundScore =
            getSingleRunErrorsScore
              sortAlg
              listLength
              wChance
              sChance
              seed
       in totalScore + currentRoundScore

getSingleRunErrorsScore ::
  ( WonkyState ->
    Sortable ->
    (Sortable, WonkyState)
  ) ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int
getSingleRunErrorsScore sortAlg listLength wChance sChance seed =
  let l = randomizeList seed (SortBit [1 .. listLength])
      wonkySt =
        WonkyState
          { wonkyChance = wChance,
            stuckChance = sChance,
            previousAnswer = 0,
            stdGen = mkStdGen seed
          }
      result = fst (sortAlg wonkySt l)
   in getTotalPositionalErrors (fromSortBit result)
