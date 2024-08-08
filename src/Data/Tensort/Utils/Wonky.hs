module Data.Tensort.Utils.Wonky
  ( handleWonkiness,
    mkWonkySt,
    setPreviousAnswer,
  )
where

import Data.Tensort.Utils.Types (WonkyState (..))
import System.Random (StdGen, mkStdGen, randomR)

mkWonkySt :: Int -> Int -> Int -> WonkyState
mkWonkySt wChance sChance stdGenSeed =
  WonkyState
    { wonkyChance = wChance,
      stuckChance = sChance,
      previousAnswer = 0,
      stdGen = mkStdGen stdGenSeed
    }

setStdGen :: WonkyState -> StdGen -> WonkyState
setStdGen wonkySt stdGen' =
  WonkyState
    { wonkyChance = wonkyChance wonkySt,
      stuckChance = stuckChance wonkySt,
      previousAnswer = previousAnswer wonkySt,
      stdGen = stdGen'
    }

setPreviousAnswer :: WonkyState -> Int -> WonkyState
setPreviousAnswer wonkySt pAnswer' =
  WonkyState
    { wonkyChance = wonkyChance wonkySt,
      stuckChance = stuckChance wonkySt,
      previousAnswer = pAnswer',
      stdGen = stdGen wonkySt
    }

handleWonkiness :: WonkyState -> (Bool, Int, WonkyState)
handleWonkiness wonkySt = do
  let (roll, stdGen') = randomR (1 :: Int, 100) (stdGen wonkySt)
  let (roll', stdGen'') = randomR (1 :: Int, 100) stdGen'
  let wonkySt' = setStdGen wonkySt stdGen''
  if roll <= stuckChance wonkySt'
    then (True, previousAnswer wonkySt', wonkySt')
    else
      if roll' <= wonkyChance wonkySt'
        then do
          let (rndResult, stdGen''') = randomR (-1 :: Int, 1) (stdGen wonkySt')
          (True, rndResult, setStdGen wonkySt' stdGen''')
        else (False, -2, wonkySt')
