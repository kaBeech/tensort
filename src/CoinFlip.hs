module CoinFlip (flipTenCoins) where

import Control.Monad.State
import System.Random (Random (random), RandomGen, StdGen, mkStdGen)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

tenCoins :: State StdGen (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
tenCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  d <- randomSt
  e <- randomSt
  f <- randomSt
  g <- randomSt
  h <- randomSt
  i <- randomSt
  j <- randomSt
  return (a, b, c, d, e, f, g, h, i, j)

flipTenCoins :: Int -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
flipTenCoins seed = evalState tenCoins (mkStdGen seed)
