module PrintErrorSpread (printErrorSpread) where

import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Types
  ( Sortable (..),
    WonkyState (..),
  )
import SubAlgsCompared (subAlgsCompared)
import System.Random (mkStdGen)

printErrorSpread :: Int -> IO ()
printErrorSpread i = foldr acc (return ()) subAlgsCompared
  where
    acc sortAlg io = do
      _ <- io
      printErrorSpreadForAlg i sortAlg

printErrorSpreadForAlg ::
  Int ->
  (WonkyState -> Sortable -> (Sortable, WonkyState), String) ->
  IO ()
printErrorSpreadForAlg i (sortAlg, sortName) = do
  putStrLn (sortName ++ " Error Spread")
  putStrLn "-------------------------"
  let (abc, acb, bac, bca, cab, cba) =
        getErrorSpread (0, 0, 0, 0, 0, 0) [1 .. i] sortAlg
  putStrLn ("123: " ++ show abc)
  putStrLn ("132: " ++ show acb)
  putStrLn ("213: " ++ show bac)
  putStrLn ("231: " ++ show bca)
  putStrLn ("312: " ++ show cab)
  putStrLn ("321: " ++ show cba)
  putStrLn ""

getErrorSpread ::
  (Int, Int, Int, Int, Int, Int) ->
  [Int] ->
  (WonkyState -> Sortable -> (Sortable, WonkyState)) ->
  (Int, Int, Int, Int, Int, Int)
getErrorSpread errorSpread [] _ = errorSpread
getErrorSpread errorSpread (x : xs) sortAlg =
  let (abc, acb, bac, bca, cab, cba) = getErrorSpread errorSpread xs sortAlg
      l = randomizeList x (SortBit [1, 2, 3])
      wonkySt =
        WonkyState
          { wonkyChance = 10,
            stuckChance = 0,
            previousAnswer = 0,
            stdGen = mkStdGen x
          }
      result = fst (sortAlg wonkySt l)
   in switch (abc, acb, bac, bca, cab, cba) result
  where
    switch (abc, acb, bac, bca, cab, cba) result
      | result == SortBit [1, 2, 3] =
          (abc + 1, acb, bac, bca, cab, cba)
      | result == SortBit [1, 3, 2] =
          (abc, acb + 1, bac, bca, cab, cba)
      | result == SortBit [2, 1, 3] =
          (abc, acb, bac + 1, bca, cab, cba)
      | result == SortBit [2, 3, 1] =
          (abc, acb, bac, bca + 1, cab, cba)
      | result == SortBit [3, 1, 2] =
          (abc, acb, bac, bca, cab + 1, cba)
      | result == SortBit [3, 2, 1] =
          (abc, acb, bac, bca, cab, cba + 1)
      | otherwise =
          error
            ( "From getErrorSpread: Unrecognized result: "
                ++ show result
            )
