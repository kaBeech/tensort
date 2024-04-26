module Wonky (trinaryCompare) where

import Control.Monad.State
import System.Random (Random (random), RandomGen, StdGen, mkStdGen)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

getRandomNumber :: Int -> State StdGen Int
getRandomNumber maxNumber = do
  randomNumber <- randomSt
  return (1 + (randomNumber `mod` maxNumber))

compareRandomly :: State StdGen Int
compareRandomly = do
  randomResult <- getRandomNumber 3
  return (randomResult - 2)

rollPercentChance :: Int -> State StdGen Bool
rollPercentChance percentage = do
  roll <- getRandomNumber 100
  return (roll <= percentage)

trinaryCompare :: (Ord a) => a -> a -> Int -> Int -> Int
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
