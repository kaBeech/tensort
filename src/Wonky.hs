module Wonky (trinaryCompare, greaterThan, lessThan) where

import Control.Monad.State
import Pseudorandom (getRandomNumber, rollPercentChance)
import System.Random (StdGen, mkStdGen)

compareRandomly :: State StdGen Int
compareRandomly = do
  randomResult <- getRandomNumber 3
  return (randomResult - 2)

trinaryCompare :: Int -> Int -> Int -> Int -> Int
trinaryCompare x y wonkinessPercentage seed = do
  if evalState (rollPercentChance wonkinessPercentage) (mkStdGen seed)
    then do
      evalState compareRandomly (mkStdGen seed)
    else
      if x < y
        then -1
        else
          if x == y
            then 0
            else 1

greaterThan :: Int -> Int -> Int -> Int -> Bool
greaterThan x y wonkinessPercentage seed = do
  trinaryCompare x y wonkinessPercentage seed == 1

lessThan :: Int -> Int -> Int -> Int -> Bool
lessThan x y wonkinessPercentage seed = do
  trinaryCompare x y wonkinessPercentage seed == -1
