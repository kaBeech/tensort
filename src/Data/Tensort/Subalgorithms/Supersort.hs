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
  )

supersort ::
  (SortAlg, SortAlg, SortAlg, SupersortStrat) ->
  Sortable ->
  Sortable
supersort (subAlg1, subAlg2, subAlg3, superStrat) xs = do
  let result1 = subAlg1 xs
  let result2 = subAlg2 xs
  if result1 == result2
    then result1
    else superStrat (result1, result2, subAlg3 xs)

mundaneSuperStrat :: SupersortStrat
mundaneSuperStrat (result1, result2, result3) = if result2 == result3 then result2 else result1

-- Previously we used different SuperStrats for Mundane and Magic Supersorts.
-- Currently there is no need to differentiate, but we keep this here in case
-- this changes again in the future
magicSuperStrat :: SupersortStrat
magicSuperStrat = mundaneSuperStrat
