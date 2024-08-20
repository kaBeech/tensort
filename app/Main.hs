module Main where

import Benchmarking.PrintDeckShuffleErrors (printDeckShuffleErrors)
import Benchmarking.PrintErrorRateComparison (printErrorRateComparison)
import Benchmarking.PrintErrorSpread (printErrorSpread)
import Benchmarking.PrintTimes (printTimes)

wonkyChance :: Int
wonkyChance = 10

stuckChance :: Int
stuckChance = 0

main :: IO ()
main = do
  printErrorRateComparison 1000 wonkyChance stuckChance
  printErrorSpread 1000 wonkyChance stuckChance
  printDeckShuffleErrors 1000 wonkyChance stuckChance
  printTimes [3 .. 12] 10 wonkyChance stuckChance
