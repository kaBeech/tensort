-- | This module provides functions to reduce a list of TensorStacks into a
--   more compact list of TensorStacks.
module Data.Tensort.Utils.Reduce (reduceTensorStacks) where

import Data.Tensort.Utils.Compose (createTensor)
import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types
  ( Memory (..),
    TensorStack,
    TensortProps (..),
  )

-- | Take a list of TensorStacks and group them together in new
--   TensorStacks, each containing bytesize number of Tensors (former
--   TensorStacks), until the number of TensorStacks is equal to the bytesize.

--   The Registers of the new TensorStacks are bubblesorted, as usual.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> reduceTensorStacks (mkTsProps 2 bubblesort) [([(0, 33), (1, 38)], ByteMem [[31, 33], [35, 38]]), ([(0, 34), (1, 37)], ByteMem [[32, 14], [36, 37]]), ([(0, 23), (1, 27)], ByteMem [[21, 23], [25, 27]]), ([(0, 24), (1, 28)], ByteMem [[22, 24], [26, 28]]),([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
-- ([(1,18),(0,38)],TensorMem [([(0,28),(1,38)],TensorMem [([(0,27),(1,28)],TensorMem [([(0,23),(1,27)],ByteMem [[21,23],[25,27]]),([(0,24),(1,28)],ByteMem [[22,24],[26,28]])]),([(1,37),(0,38)],TensorMem [([(0,33),(1,38)],ByteMem [[31,33],[35,38]]),([(0,34),(1,37)],ByteMem [[32,14],[36,37]])])]),([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])])])
reduceTensorStacks :: (Ord a) => TensortProps a -> [TensorStack a] -> TensorStack a
reduceTensorStacks tsProps tensorStacks =
  if length newTensorStacks <= bytesize tsProps
    then createTensor subAlg memory
    else reduceTensorStacks tsProps newTensorStacks
  where
    subAlg = subAlgorithm tsProps
    memory = TensorMem tensorStacks
    newTensorStacks = reduceTensorStacksSinglePass tsProps tensorStacks

-- | Take a list of TensorStacks and group them together in new
--   TensorStacks each containing bytesize number of Tensors (former
--   TensorStacks).

--   The Registers of the new TensorStacks are bubblesorted, as usual.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> reduceTensorStacksSinglePass (mkTsProps 2 bubblesort) [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
-- [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])]
reduceTensorStacksSinglePass ::
  (Ord a) =>
  TensortProps a ->
  [TensorStack a] ->
  [TensorStack a]
reduceTensorStacksSinglePass tsProps tensorStacks =
  foldr acc [] tensorStacks'
  where
    tensorStacks' = splitEvery (bytesize tsProps) tensorStacks
    acc tensorStack newTensorStacks =
      newTensorStacks
        ++ [newTensorStack]
      where
        newTensorStack = createTensor subAlg memory
        subAlg = subAlgorithm tsProps
        memory = TensorMem tensorStack
