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
    else do
      let (result3, wonkySt'') = subAlg3 wonkySt' xs
      (superStrat (result1, result2, result3), wonkySt'')

mundaneSuperStrat :: SupersortStrat
mundaneSuperStrat (SortBit result1, SortBit result2, SortBit result3) = do
  if result2 == result3 then SortBit result3 else SortBit result1
mundaneSuperStrat (SortRec result1, SortRec result2, SortRec result3) = do
  if result2 == result3 then SortRec result3 else SortRec result1
mundaneSuperStrat (_, _, _) = error "From Mundane SuperStrat: All three inputs must be of the same type."

magicSuperStrat :: SupersortStrat
magicSuperStrat (SortBit _result1, SortBit _result2, SortBit result3) = SortBit result3
magicSuperStrat (SortRec _result1, SortRec _result2, SortRec result3) = SortRec result3
magicSuperStrat (_, _, _) = error "From Magic SuperStrat: All three inputs must be of the same type."

-- Something like the following could also be considered. This would be less
-- reliable in cases where only the element comparison module is faulty, but
-- could be more reliable in cases where both the element comparison module and
-- the list comparison module are faulty
--
-- mundaneSuperStrat :: SupersortStrat
-- mundaneSuperStrat (SortBit result1, SortBit result2, SortBit result3) = do
--   if result1 == result3 || result2 == result3
--     then SortBit result3
--     else
--       if equalTo (last result1) (last result2) || equalTo (last result1) (last result3)
--         then SortBit result1
--         else
--           if equalTo (last result2) (last result3)
--             then SortBit result2
--             else SortBit result1
-- mundaneSuperStrat (SortRec result1, SortRec result2, SortRec result3) = do
--   if result1 == result3 || result2 == result3
--     then SortRec result3
--     else
--       if equalTo (last result1) (last result2) || equalTo (last result1) (last result3)
--         then SortRec result1
--         else
--           if equalTo (last result2) (last result3)
--             then SortRec result2
--             else SortRec result1
-- mundaneSuperStrat (_, _, _) = error "From Mundane SuperStrat: All three inputs must be of the same type."
--
-- magicSuperStrat :: SupersortStrat
-- magicSuperStrat (SortBit result1, SortBit result2, SortBit result3) = do
--   if equalTo (last result1) (last result3) || equalTo (last result2) (last result3)
--     then SortBit result3
--     else
--       if equalTo (last result1) (last result2)
--         then SortBit result1
--         else SortBit result3
-- magicSuperStrat (SortRec result1, SortRec result2, SortRec result3) = do
--   if equalTo (last result1) (last result3) || equalTo (last result2) (last result3)
--     then SortRec result3
--     else
--       if equalTo (last result1) (last result2)
--         then SortRec result1
--         else SortRec result3
-- magicSuperStrat (_, _, _) = error "From Magic SuperStrat: All three inputs must be of the same type."
