module Main where

import Benchmarking.PrintDeckShuffleErrors (printDeckShuffleErrors)
import Benchmarking.PrintErrorSpread (printErrorSpread)
import Benchmarking.PrintLargeTimeAndErrorRateComparison (printLargeTimeAndErrorRateComparison)
import Benchmarking.PrintSubAlgErrorRateComparison (printSubAlgErrorRateComparison)

wonkyChance :: Int
wonkyChance = 10

stuckChance :: Int
stuckChance = 0

main :: IO ()
main = do
  printSubAlgErrorRateComparison 1000 wonkyChance stuckChance
  printErrorSpread 1000 wonkyChance stuckChance
  printDeckShuffleErrors 1000 wonkyChance stuckChance
  printLargeTimeAndErrorRateComparison [3 .. 12] 10 wonkyChance stuckChance
