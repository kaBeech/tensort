-- | This module provides functions for creating Supersort variants for
--   adjudicating between 3 sorting algorithms
module Data.Tensort.Subalgorithms.Supersort
  ( supersort,
    mundaneSuperStrat,
    magicSuperStrat,
  )
where

import Data.Tensort.Utils.Types
  ( SortAlg,
    Sortable (..),
    SupersortStrat,
  )

-- | Used for creating a Supersort algorithm that adjudicates between 3 sorting
--   algorithms
--
--   Takes 3 sorting algorithms and a SuperStrat and returns a SortAlg that
--   adjudicates between the 3 sorting algorithms using the provided SuperStrat

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
-- >>> import Data.Tensort.OtherSorts.Mergesort (mergesort)
--
-- >>> supersort (mergesort, bubblesort, permutationsort, mundaneSuperStrat) (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> supersort (mergesort, bubblesort, permutationsort, mundaneSuperStrat) (SortRec [(16, 23), (4, 8), (15, 42)])
-- SortRec [(4,8),(16,23),(15,42)]
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

-- | Takes 3 SortAlgs and adjudicates between them to find a common result to
--   increase robustness

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.OtherSorts.Mergesort (mergesort)
-- >>> import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
--
-- >>> supersort (mergesort, bubblesort, permutationsort, mundaneSuperStrat) (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> supersort (mergesort, bubblesort, permutationsort, mundaneSuperStrat) (SortRec [(16, 23), (4, 8), (15, 42)])
-- SortRec [(4,8),(16,23),(15,42)]
mundaneSuperStrat :: SupersortStrat
mundaneSuperStrat (result1, result2, result3) = if result2 == result3 then result2 else result1

-- | Takes 3 SortAlgs and adjudicates between them to find a common result to
--   increase robustness
--
--   Previously we used different SuperStrats for Mundane and Magic Supersorts.
--   Currently there is no need to differentiate, but we keep this here for
--   backwards compatibility and in case this changes again in the future

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.OtherSorts.Mergesort (mergesort)
-- >>> import Data.Tensort.Subalgorithms.Permutationsort (permutationsort)
--
-- >>> supersort (mergesort, bubblesort, permutationsort, magicSuperStrat) (SortBit [16, 23, 4, 8, 15, 42])
-- SortBit [4,8,15,16,23,42]
--
-- >>> supersort (mergesort, bubblesort, permutationsort, magicSuperStrat) (SortRec [(16, 23), (4, 8), (15, 42)])
-- SortRec [(4,8),(16,23),(15,42)]
magicSuperStrat :: SupersortStrat
magicSuperStrat = mundaneSuperStrat
