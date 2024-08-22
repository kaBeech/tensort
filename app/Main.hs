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
wonkyChance = 10 -- Percent chance that comparing 2 Bits will yield a random result

stuckChance :: Int
stuckChance = 0 -- Percent chance that comparing 2 Bits will yield the same result as previously given

main :: IO ()
main = do
  -- makeDeckShuffleChart
  -- printSubAlgErrorRateComparison 1000 wonkyChance stuckChance
  -- printErrorSpread 1000 wonkyChance stuckChance
  -- printDeckShuffleErrors 1000 wonkyChance stuckChance
  -- printLargeTimeAndErrorRateComparison
  --   [3 .. 14] -- n, where list size = 2^n
  --   100 -- Number of runs per SortAlg/list length
  --   1024 -- Bubblesort cutoff - don't benchmark Bubblesort after this list length
  --   wonkyChance
  --   stuckChance
  printGreeting

printGreeting :: IO ()
printGreeting =
  print
    "Welcome! To run the benchmarks, please uncomment the \
    \desired function in `app/Main.hs`, tweak any settings desired, and run \
    \the program."
