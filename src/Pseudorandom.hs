module Pseudorandom (getRandomNumber, rollPercentChance) where

import Control.Monad.State
import System.Random (Random (random), RandomGen, StdGen, mkStdGen)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

getRandomNumber :: Int -> State StdGen Int
getRandomNumber maxNumber = do
  randomNumber <- randomSt
  return (1 + (randomNumber `mod` maxNumber))

rollPercentChance :: Int -> State StdGen Bool
rollPercentChance percentage = do
  roll <- getRandomNumber 100
  return (roll <= percentage)
