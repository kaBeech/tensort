module Main where

import CoinFlip qualified (flipTenCoins)
import RobustsortHybrid qualified (rsorthbm, rsorthbq, rsorthmq)
import RobustsortParallel qualified (rsortpb, rsortpm, rsortpq)
import RobustsortSeries qualified (rsortsb, rsortsm, rsortsq)
import Vanillasort qualified (bsort, msort, qsort)
import Wonky qualified (trinaryCompare)

unsortedArray :: [String]
unsortedArray = ["b", "e", "j", "d", "o", "k", "g", "n", "p", "f", "m", "c", "h", "i", "l", "a"]

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ("bsort: " ++ unwords (Vanillasort.bsort unsortedArray))
  putStrLn ("msort: " ++ unwords (Vanillasort.msort unsortedArray))
  putStrLn ("qsort: " ++ unwords (Vanillasort.qsort unsortedArray))
  putStrLn ("rsortsb: " ++ unwords (RobustsortSeries.rsortsb unsortedArray))
  putStrLn ("rsortsm: " ++ unwords (RobustsortSeries.rsortsm unsortedArray))
  putStrLn ("rsortsq: " ++ unwords (RobustsortSeries.rsortsq unsortedArray))
  RobustsortParallel.rsortpb
  RobustsortParallel.rsortpm
  putStrLn ("rsortpq: " ++ unwords (RobustsortParallel.rsortpq unsortedArray))
  putStrLn ("rsorthbm: " ++ unwords (RobustsortHybrid.rsorthbm unsortedArray))
  putStrLn ("rsorthbq: " ++ unwords (RobustsortHybrid.rsorthbq unsortedArray))
  putStrLn ("rsorthmq: " ++ unwords (RobustsortHybrid.rsorthmq unsortedArray))
  putStrLn ("Flip Ten Coins: " ++ show (CoinFlip.flipTenCoins 3343))
  putStrLn ("Trinary Compare: " ++ show (Wonky.trinaryCompare 1 2 50 345))
