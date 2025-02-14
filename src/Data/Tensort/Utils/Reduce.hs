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
-- >>> import Data.Tensort.Utils.Types (Record (..))
-- >>> import Data.Tensort.Utils.Types (Tensor (..))
-- >>> reduceTensorStacks (mkTsProps 2 bubblesort) [Tensor ([Record (0 :: Int, 33), Record (1 :: Int, 38)], ByteMem [[31, 33] :: [Int], [35, 38] :: [Int]]), Tensor ([Record (0 :: Int, 34), Record (1 :: Int, 37)], ByteMem [[32, 14] :: [Int], [36, 37] :: [Int]]), Tensor ([Record (0 :: Int, 23), Record (1 :: Int, 27)], ByteMem [[21, 23] :: [Int], [25, 27] :: [Int]]), Tensor ([Record (0 :: Int, 24), Record (1 :: Int, 28)], ByteMem [[22, 24] :: [Int], [26, 28] :: [Int]]),Tensor ([Record (0 :: Int,13),Record (1 :: Int,18)],ByteMem [[11,13] :: [Int],[15,18] :: [Int]]),Tensor ([Record (0 :: Int,14),Record (1 :: Int,17)],ByteMem [[12,14] :: [Int],[16,17] :: [Int]]),Tensor ([Record (0 :: Int,3),Record (1 :: Int,7)],ByteMem [[1,3] :: [Int],[5,7] :: [Int]]),Tensor ([Record (0 :: Int,4),Record (1 :: Int,8)],ByteMem [[2,4] :: [Int],[6,8] :: [Int]])]
-- Tensor ([Record (1,0),Record (1,1),Record (1,2),Record (1,3)],TensorMem [Tensor ([Record (1,0),Record (1,1)],TensorMem [Tensor ([Record (0,3),Record (1,7)],ByteMem [[1,3],[5,7]]),Tensor ([Record (0,4),Record (1,8)],ByteMem [[2,4],[6,8]])]),Tensor ([Record (1,0),Record (1,1)],TensorMem [Tensor ([Record (0,13),Record (1,18)],ByteMem [[11,13],[15,18]]),Tensor ([Record (0,14),Record (1,17)],ByteMem [[12,14],[16,17]])]),Tensor ([Record (1,0),Record (1,1)],TensorMem [Tensor ([Record (0,23),Record (1,27)],ByteMem [[21,23],[25,27]]),Tensor ([Record (0,24),Record (1,28)],ByteMem [[22,24],[26,28]])]),Tensor ([Record (1,0),Record (1,1)],TensorMem [Tensor ([Record (0,33),Record (1,38)],ByteMem [[31,33],[35,38]]),Tensor ([Record (0,34),Record (1,37)],ByteMem [[32,14],[36,37]])])])
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
-- >>> import Data.Tensort.Utils.Types (Record (..))
-- >>> import Data.Tensort.Utils.Types (Tensor (..))
-- >>> reduceTensorStacksSinglePass (mkTsProps 2 bubblesort) [Tensor ([Record (0 :: Int,13),Record (1 :: Int,18)],ByteMem [[11,13] :: [Int],[15,18] :: [Int]]),Tensor ([Record (0 :: Int,14),Record (1 :: Int,17)],ByteMem [[12,14] :: [Int],[16,17] :: [Int]]),Tensor ([Record (0 :: Int,3),Record (1 :: Int,7)],ByteMem [[1,3] :: [Int],[5,7] :: [Int]]),Tensor ([Record (0 :: Int,4),Record (1 :: Int,8)],ByteMem [[2,4] :: [Int],[6,8] :: [Int]])]
-- [Tensor ([Record (1,0),Record (1,1)],TensorMem [Tensor ([Record (0,3),Record (1,7)],ByteMem [[1,3],[5,7]]),Tensor ([Record (0,4),Record (1,8)],ByteMem [[2,4],[6,8]])]),Tensor ([Record (1,0),Record (1,1)],TensorMem [Tensor ([Record (0,13),Record (1,18)],ByteMem [[11,13],[15,18]]),Tensor ([Record (0,14),Record (1,17)],ByteMem [[12,14],[16,17]])])]
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
