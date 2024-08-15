module Data.Tensort.Subalgorithms.Supersort
  ( supersort,
    mundaneSuperStrat,
    magicSuperStrat,
  )
where

import Data.Tensort.Utils.ComparisonFunctions (equalBit, equalRecord)
import Data.Tensort.Utils.Types (SortAlg, Sortable (..), SupersortStrat, WonkyState, fromSortBit, fromSortRec)

supersort :: (SortAlg, SortAlg, SortAlg, SupersortStrat) -> WonkyState -> Sortable -> (Sortable, WonkyState)
supersort (subAlg1, subAlg2, subAlg3, superStrat) wonkySt (SortBit xs) = do
  let (result1, _) = subAlg1 wonkySt (SortBit xs)
  let (result2, wonkySt') = subAlg2 wonkySt (SortBit xs)
  if result1 == result2
    then (result1, wonkySt')
    else do
      let (result, wonkySt'') =
            equalBit
              (last (fromSortBit result1))
              (last (fromSortBit result2))
              wonkySt'
      if result
        then (result2, wonkySt'')
        else do
          let (result3, wonkySt''') = subAlg3 wonkySt'' (SortBit xs)
          (superStrat (result1, result2, result3), wonkySt''')
supersort (subAlg1, subAlg2, subAlg3, superStrat) wonkySt (SortRec xs) = do
  let (result1, _) = subAlg1 wonkySt (SortRec xs)
  let (result2, wonkySt') = subAlg2 wonkySt (SortRec xs)
  if result1 == result2
    then (result1, wonkySt')
    else do
      let (result, wonkySt'') =
            equalRecord
              (last (fromSortRec result1))
              (last (fromSortRec result2))
              wonkySt'
      if result
        then (result2, wonkySt'')
        else do
          let (result3, wonkySt''') = subAlg3 wonkySt'' (SortRec xs)
          (superStrat (result1, result2, result3), wonkySt''')

mundaneSuperStrat :: SupersortStrat
mundaneSuperStrat (SortBit result1, SortBit result2, SortBit result3) = do
  if result2 == result3 then SortBit result2 else SortBit result1
mundaneSuperStrat (SortRec result1, SortRec result2, SortRec result3) = do
  if result2 == result3 then SortRec result2 else SortRec result1
mundaneSuperStrat (_, _, _) =
  error
    "From Mundane SuperStrat: All three inputs must be of the same type."

-- Previously we used different SuperStrats for Mundane and Magic Supersorts.
-- Currently there is no need to differentiate, but we keep this here in case
-- this changes again in the future
magicSuperStrat :: SupersortStrat
magicSuperStrat = mundaneSuperStrat
