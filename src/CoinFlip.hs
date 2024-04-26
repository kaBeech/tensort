module CoinFlip (flipTenCoins) where

import Control.Monad.State
import Pseudorandom (rollPercentChance)
import System.Random (StdGen, mkStdGen)

tenCoins :: State StdGen (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
tenCoins = do
  a <- rollPercentChance 50
  b <- rollPercentChance 50
  c <- rollPercentChance 50
  d <- rollPercentChance 50
  e <- rollPercentChance 50
  f <- rollPercentChance 50
  g <- rollPercentChance 50
  h <- rollPercentChance 50
  i <- rollPercentChance 50
  j <- rollPercentChance 50
  return (a, b, c, d, e, f, g, h, i, j)

flipTenCoins :: Int -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
flipTenCoins seed = evalState tenCoins (mkStdGen seed)
