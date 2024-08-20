module Main where

import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types
  ( Sortable (..),
  )
import PrintDeckShuffleErrors (printDeckShuffleErrors)
import PrintErrorRateComparison (printErrorRateComparison)
import PrintErrorSpread (printErrorSpread)
import PrintTimes (printTimes)

genUnsortedBits :: Int -> Sortable
genUnsortedBits n = randomizeList 143 (SortBit [1 .. n])

genTestPeriod :: Int -> Int
genTestPeriod n = 2 ^ n

wonkyChance :: Int
wonkyChance = 10

stuckChance :: Int
stuckChance = 0

main :: IO ()
main = do
  printErrorRateComparison 1000 wonkyChance stuckChance
  printErrorSpread 1000 wonkyChance stuckChance
  printDeckShuffleErrors 1000 wonkyChance stuckChance
  printTimes (map (\x -> (genUnsortedBits (genTestPeriod x), 143)) [3 .. 14]) wonkyChance stuckChance
