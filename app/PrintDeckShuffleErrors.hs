module PrintDeckShuffleErrors (printDeckShuffleErrors) where

import Data.Tensort.Utils.RandomizeList (randomizeList)
import Data.Tensort.Utils.Score (getTotalPositionalErrors)
import Data.Tensort.Utils.Types
  ( Sortable (..),
    WonkyState (..),
    fromSortBit,
  )
import PadOut (padOut)
import SortAlgsCompared (sortAlgsCompared)
import System.Random (mkStdGen)

printDeckShuffleErrors :: Int -> IO ()
printDeckShuffleErrors i = foldr acc (return ()) sortAlgsCompared
  where
    acc sortAlg io = do
      _ <- io
      printErrorRateComparisonForAlg i sortAlg

printErrorRateComparisonForAlg ::
  Int ->
  (WonkyState -> Sortable -> (Sortable, WonkyState), String) ->
  IO ()
printErrorRateComparisonForAlg i (sortAlg, sortName) =
  putStrLn
    ( padOut (sortName ++ " Errors: ") 30
        ++ show (getTotalErrorsScore i sortAlg `div` i)
    )

getTotalErrorsScore ::
  Int ->
  ( WonkyState ->
    Sortable ->
    (Sortable, WonkyState)
  ) ->
  Int
getTotalErrorsScore i sortAlg = foldr acc 0 [1 .. i]
  where
    acc x totalScore = do
      let l = randomizeList x (SortBit [1 .. 56])
      let wonkySt =
            WonkyState
              { wonkyChance = 10,
                stuckChance = 0,
                previousAnswer = 0,
                stdGen = mkStdGen x
              }
      let result = fst (sortAlg wonkySt l)
      let roundScore = getTotalPositionalErrors (fromSortBit result)
      totalScore + roundScore
