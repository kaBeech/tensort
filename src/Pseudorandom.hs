module Pseudorandom (randomizeArray) where

import Control.Monad.State
import System.Random (StdGen, mkStdGen, randomR)

data RandomState where
  RandomState :: {generator :: StdGen} -> RandomState

randomizeArray :: [Int] -> [Int]
randomizeArray elements = evalState (randomizeArraySinglePass elements []) (RandomState (mkStdGen 143))

randomizeArraySinglePass :: [Int] -> [Int] -> State RandomState [Int]
randomizeArraySinglePass [] newArray = return newArray
randomizeArraySinglePass oldArray newArray = do
  random <- get
  let gen = generator random
  let (randomIndex, gen') = randomR (0, length oldArray - 1) gen
  let element = oldArray !! randomIndex
  put RandomState {generator = gen'}
  randomizeArraySinglePass (filter (/= element) oldArray) (element : newArray)
