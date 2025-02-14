-- | Module for creating Tensors from Bytes and Tensors.
--
--   Functions ending in "B" are for sorting Bits in a base (non-recursive)
--   Tensort variant.
--
--   Functions ending in "R" are for sorting Records when used in a recursive
--   Tensort variant.
module Data.Tensort.Utils.Compose
  ( createInitialTensors,
    createTensor,
  )
where

import Data.Tensort.Utils.SortRecs (sortRecs)
import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types
  ( Bit,
    Byte,
    Memory (..),
    Record (..),
    Register,
    SortAlg,
    Tensor (..),
    TensortProps (..),
    fromRecord,
    fromTensor,
  )

-- | Convert a list of Bytes to a list of TensorStacks.

-- | This is accomplished by making a Tensor for each Byte, converting that
--   Tensor into a TensorStack (these are equivalent terms - see type
--   definitions for more info) and collating the TensorStacks into a list.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> createInitialTensors (mkTsProps 2 bubblesort) [[2,4],[6,8],[1,3],[5,7]]
-- [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
createInitialTensors :: (Ord a) => TensortProps a -> [Byte a] -> [Tensor a]
createInitialTensors tsProps bytes =
  foldr acc [] (splitEvery (bytesize tsProps) bytes)
  where
    acc byte tensorStacks =
      tensorStacks
        ++ [tensorStack]
      where
        tensorStack = getTensorFromBytes subAlg byte
        subAlg = subAlgorithm tsProps

-- | Create a Tensor from a Memory.
--
--   Aliases to getTensorFromBytes for ByteMem and getTensorFromTensors for
--   TensorMem.
createTensor :: (Ord a) => SortAlg a -> Memory a -> Tensor a
createTensor subAlg (ByteMem bytes) = getTensorFromBytes subAlg bytes
createTensor subAlg (TensorMem tensors) = getTensorFromTensors subAlg tensors

-- | Convert a list of Bytes to a Tensor.

-- | We do this by loading the list of Bytes into the new Tensor's Memory
--   and adding a sorted Register containing References to each Byte in Memory.

-- | Each Record contains an Address pointing to the index of the referenced
--   Byte and a TopBit containing the value of the last (i.e. highest) Bit in
--   the referenced Byte.

-- | The Register is sorted by the TopBits of each Record.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getTensorFromBytes bubblesort [[2,4,6,8],[1,3,5,7]]
-- ([(1,7),(0,8)],ByteMem [[2,4,6,8],[1,3,5,7]])
getTensorFromBytes :: (Ord a) => SortAlg a -> [Byte a] -> Tensor a
getTensorFromBytes subAlg bytes = Tensor (register', ByteMem bytes)
  where
    register' = sortRecs subAlg $ map Record register
    register = acc bytes [] 0
    acc [] regi _ = regi
    acc ([] : remainingBytes) regi i = acc remainingBytes regi (i + 1)
    acc (byte : remainingBytes) regi i =
      acc remainingBytes (regi ++ [record]) (i + 1)
      where
        record = (last byte, i :: Int)

-- | Create a TensorStack with the collated and sorted References from the
--   Tensors as its Register and the original Tensors as its Memory.

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getTensorFromTensors bubblesort [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])]
-- ([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])])
getTensorFromTensors :: (Ord a) => SortAlg a -> [Tensor a] -> Tensor a
getTensorFromTensors subAlg tensors = Tensor (sortedRegister, TensorMem tensors)
  where
    sortedRegister = sortRecs subAlg unsortedRegister
    unsortedRegister = getRegisterFromTensors tensors

-- | Used in creating a Register for a newly-created Tensor which encloses
--   other Tensors.
--
--   Takes a list of Tensors to be processed into the enclosing Tensor's
--   memory. For each Tensor in the list, produces a Record by combining the
--   top bit of the Tensor with an index value for its Address.

-- | Note that this output is not sorted. Sorting is done in the
--   getTensorFromTensors function.

-- | ==== __Examples__
-- >>> getRegisterFromTensors [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
-- [(0,18),(1,17),(2,7),(3,8)]
getRegisterFromTensors :: [Tensor a] -> [Record a]
getRegisterFromTensors tensors = acc tensors []
  where
    acc :: [Tensor a] -> [Record a] -> [Record a]
    acc [] records = records
    acc (Tensor ([], _) : remainingTensors) records = acc remainingTensors records
    acc (tensor : remainingTensors) records =
      acc remainingTensors $ records ++ [Record record]
      where
        record = (topBit, i)
        i = length records
        topBit = getTopBitFromTensorStack (fromTensor tensor)

-- | Get the top Bit from a TensorStack.

-- | The top Bit is the last Bit in the last Byte referenced in the last record
--   of the Tensor referenced in the last record of the last Tensor of...
--   and so on until you reach the top level of the TensorStack.

-- | This is also expected to be the highest value in the TensorStack.

-- | ==== __Examples__
-- >>> getTopBitFromTensorStack ([(0,28),(1,38)],TensorMem [([(0,27),(1,28)],TensorMem [([(0,23),(1,27)],ByteMem [[21,23],[25,27]]),([(0,24),(1,28)],ByteMem [[22,24],[26,28]])]),([(1,37),(0,38)],TensorMem [([(0,33),(1,38)],ByteMem [[31,33],[35,38]]),([(0,34),(1,37)],ByteMem [[32,14],[36,37]])])])
-- 38
getTopBitFromTensorStack :: (Register a, Memory a) -> Bit a
getTopBitFromTensorStack tensor =
  let register = fst tensor
      topRecord = last register
      topBit = fst (fromRecord topRecord)
   in topBit
