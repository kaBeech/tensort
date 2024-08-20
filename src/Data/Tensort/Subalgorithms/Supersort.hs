module Data.Tensort.Subalgorithms.Supersort
  ( supersort,
    mundaneSuperStrat,
    magicSuperStrat,
  )
where

import Data.Tensort.Utils.Types (SortAlg, Sortable (..), SupersortStrat, WonkyState)

supersort :: (SortAlg, SortAlg, SortAlg, SupersortStrat) -> WonkyState -> Sortable -> (Sortable, WonkyState)
supersort (subAlg1, subAlg2, subAlg3, superStrat) wonkySt xs = do
  let (result1, _) = subAlg1 wonkySt xs
  let (result2, wonkySt') = subAlg2 wonkySt xs
  if result1 == result2
    then (result1, wonkySt')
    else
      let (result3, wonkySt'') = subAlg3 wonkySt' xs
       in (superStrat (result1, result2, result3), wonkySt'')

mundaneSuperStrat :: SupersortStrat
mundaneSuperStrat (result1, result2, result3) = if result2 == result3 then result2 else result1

-- Previously we used different SuperStrats for Mundane and Magic Supersorts.
-- Currently there is no need to differentiate, but we keep this here in case
-- this changes again in the future
magicSuperStrat :: SupersortStrat
magicSuperStrat = mundaneSuperStrat
