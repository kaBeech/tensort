module Main where

import Benchmarking.MakeDeckShuffleChart (makeDeckShuffleChart)
import Benchmarking.PrintDeckShuffleErrors (printDeckShuffleErrors)
import Benchmarking.PrintErrorSpread (printErrorSpread)
import Benchmarking.PrintLargeTimeAndErrorRateComparison
  ( printLargeTimeAndErrorRateComparison,
  )
import Benchmarking.PrintSubAlgErrorRateComparison
  ( printSubAlgErrorRateComparison,
  )

wonkyChance :: Int
wonkyChance = 10

stuckChance :: Int
stuckChance = 0

main :: IO ()
main = do
  -- makeDeckShuffleChart
  -- printSubAlgErrorRateComparison 1000 wonkyChance stuckChance
  -- printErrorSpread 1000 wonkyChance stuckChance
  -- printDeckShuffleErrors 1000 wonkyChance stuckChance
  -- printGreeting
  printLargeTimeAndErrorRateComparison
    [12]
    1
    4096
    wonkyChance
    stuckChance

printGreeting :: IO ()
printGreeting =
  print
    "Welcome! To run the benchmarks, please uncomment the \
    \desired function in `app/Main.hs`, tweak any settings desired, and run \
    \the program."
