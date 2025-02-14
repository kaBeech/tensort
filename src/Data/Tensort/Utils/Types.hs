{-# LANGUAGE GADTs #-}

-- | This module provides types used in the Tensort package.
module Data.Tensort.Utils.Types where

-- | TensortProps contains the Bytesize and SubAlgorithm used in a Tensort
--   algorithm.
data TensortProps = TensortProps
  { bytesize :: Int,
    subAlgorithmBits :: SortAlg Bit,
    subAlgorithmRecs :: SortAlg Record
  }

-- | A Bit is a single element of the list to be sorted.
type Bit = Ordering

-- | A Byte is a list of Bits standardized to a fixed maximum length (Bytesize).

--   The length should be set either in or upstream of any function that uses
--   Bytes.
type Byte = [Bit]

-- | An Address is a index number pointing to data stored in Memory.
type Address = Int

-- | A TopBit contains a copy of the last (i.e. highest) Bit in a Byte or
--   Tensor.
type TopBit = Bit

-- | A Record is an element in a Tensor's Register
--   containing an Address pointer and a TopBit value.

--   A Record's Address is an index number pointing to a Byte or Tensor in
--   the Tensor's Memory.

--   A Record's TopBit is a copy of the last (i.e. highest) Bit in the Byte or
--   Tensor that the Record references.
type Record = (TopBit, Address)

-- | A Register is a list of Records allowing for easy access to data in a
--   Tensor's Memory.
type Register = [Record]

-- | A Memory contains the data to be sorted, either in the form of Bytes or
--   Tensors.
data Memory
  = ByteMem [Byte]
  | TensorMem [Tensor]
  deriving (Show, Eq, Ord)

-- | A Tensor contains data to be sorted in a structure allowing for
--   easy access. It consists of a Register and its Memory.

--   The Memory is a list of the Bytes or other Tensors that this Tensor
--   contains.

--   The Register is a list of Records referencing the top Bits in Memory.
type Tensor a = (Register a, Memory a)

fromTensor :: Tensor a -> ([(TopBit a, Address)], Memory a)
fromTensor (r, ByteMem m) = (map fromRecord r, ByteMem m)
fromTensor (r, TensorMem m) = (map fromRecord r, TensorMem m)

-- | A TensorStack is a top-level Tensor. In the final stages of Tensort, the
--   number of TensorStacks will be equal to (or sometimes less than) the
--   bytesize, but before that time there are expected to be many more
--   TensorStacks.
type TensorStack a = Tensor a

-- | A sorting algorithm is a function that takes a list of ordered elements
--   and returns that list sorted.
type SortAlg a = [a] -> [a]

-- | SupersortProps consist of three sorting algorithms to adjuditcate between
--   and a SupersortStrat that does the adjudication.
type SupersortProps a = (SortAlg a, SortAlg a, SortAlg a, SupersortStrat a)

-- | A SupersortStrat takes three Sortables and determines which of the three
--   is most likely to be in the correct order.
type SupersortStrat a = ([a], [a], [a]) -> [a]

-- | Converts a Maybe into a value or throws an error if the Maybe is Nothing.
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
