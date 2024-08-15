module Data.Tensort.Subalgorithms.Supersort
  ( supersort,
    mundaneSuperStrat,
    magicSuperStrat,
  )
where

import Data.Tensort.Utils.ComparisonFunctions (equalBit, equalRecord)
import Data.Tensort.Utils.Types
  ( SortAlg,
    Sortable (..),
    SupersortStrat,
    fromSortBit,
    fromSortRec,
  )

supersort ::
  (SortAlg, SortAlg, SortAlg, SupersortStrat) ->
  Sortable ->
  Sortable
supersort (subAlg1, subAlg2, subAlg3, superStrat) (SortBit xs) = do
  let result1 = subAlg1 (SortBit xs)
  let result2 = subAlg2 (SortBit xs)
  if result1 == result2
    then result1
    else
      if equalBit (last (fromSortBit result1)) (last (fromSortBit result2))
        then result2
        else superStrat (result1, result2, subAlg3 (SortBit xs))
supersort (subAlg1, subAlg2, subAlg3, superStrat) (SortRec xs) = do
  let result1 = subAlg1 (SortRec xs)
  let result2 = subAlg2 (SortRec xs)
  if result1 == result2
    then result1
    else
      if equalRecord (last (fromSortRec result1)) (last (fromSortRec result2))
        then result2
        else superStrat (result1, result2, subAlg3 (SortRec xs))

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
