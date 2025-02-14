-- | Module for creating Tensors from Bytes and Tensors.
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
-- >>> createInitialTensors (mkTsProps 2 bubblesort) [[2,4] :: [Int],[6,8] :: [Int],[1,3] :: [Int],[5,7] :: [Int]]
-- [Tensor ([Record (3,0),Record (7,1)],ByteMem [[1,3],[5,7]]),Tensor ([Record (4,0),Record (8,1)],ByteMem [[2,4],[6,8]])]
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
-- >>> getTensorFromBytes bubblesort [[2,4,6,8] :: [Int],[1,3,5,7] :: [Int]]
-- Tensor ([Record (7,1),Record (8,0)],ByteMem [[2,4,6,8],[1,3,5,7]])
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
-- >>> getTensorFromTensors bubblesort [Tensor ([Record (0,13),Record (1,18)],ByteMem [[11,13] :: [Int],[15,18] :: [Int]]),Tensor ([Record (1,14),Record (0,17)],ByteMem [[16,17] :: [Int],[12,14] :: [Int]])]
-- Tensor ([Record (0,1),Record (1,0)],TensorMem [Tensor ([Record (0,13),Record (1,18)],ByteMem [[11,13],[15,18]]),Tensor ([Record (1,14),Record (0,17)],ByteMem [[16,17],[12,14]])])
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
-- >>> getRegisterFromTensors [Tensor ([Record (0 :: Int,13),Record (1 :: Int,18)],ByteMem [[11,13] :: [Int],[15,18] :: [Int]]),Tensor ([Record (0 :: Int,14),Record (1 :: Int,17)],ByteMem [[12,14] :: [Int],[16,17] :: [Int]]),Tensor ([Record (0 :: Int,3),Record (1 :: Int,7)],ByteMem [[1,3] :: [Int],[5,7] :: [Int]]),Tensor ([Record (0 :: Int,4),Record (1 :: Int,8)],ByteMem [[2,4] :: [Int],[6,8] :: [Int]])]
-- [Record (1,0),Record (1,1),Record (1,2),Record (1,3)]
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
        topBit = getTopBitFromTensorStack tensor

-- | Get the top Bit from a TensorStack.

-- | The top Bit is the last Bit in the last Byte referenced in the last record
--   of the Tensor referenced in the last record of the last Tensor of...
--   and so on until you reach the top level of the TensorStack.

-- | This is also expected to be the highest value in the TensorStack.

-- | ==== __Examples__
-- >>> getTopBitFromTensorStack ([Record (0 :: Int,28),Record (1 :: Int,38)],TensorMem [Tensor ([Record (0 :: Int,27),Record (1 :: Int,28)],TensorMem [Tensor ([Record (0 :: Int,23),Record (1 :: Int,27)],ByteMem [[21,23] :: [Int],[25,27] :: [Int]]),Tensor ([Record (0 :: Int,24),Record (1 :: Int,28)],ByteMem [[22,24] :: [Int],[26,28] :: [Int]])]),Tensor ([Record (1 :: Int,37),Record (0 :: Int,38)],TensorMem [Tensor ([Record (0 :: Int,33),Record (1 :: Int,38)],ByteMem [[31,33] :: [Int],[35,38] :: [Int]]),Tensor ([Record (0 :: Int,34),Record (1 :: Int,37)],ByteMem [[32,14] :: [Int],[36,37] :: [Int]])])])
-- 1
getTopBitFromTensorStack :: Tensor a -> Bit a
getTopBitFromTensorStack tensor =
  let register = fst $ fromTensor tensor
      topRecord = last register
      topBit = fst (fromRecord topRecord)
   in topBit
