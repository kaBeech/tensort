{-# LANGUAGE GADTs #-}

module Data.Tensort.Utils.Types where

--   All the data types used in the Tensort and Tensort algorithms are
--   defined here. Since these packages are only for sorting Ints currently,
--   every data type is a structure of Ints

--   I know this might sound confusing, but in a recursive algorithm like this
--   it's helpful to have different names for the same type of data depending
--   on how it's being used, while still being able to use the same data in
--   multiple contexts

-- | A Bit is a single element of the list to be sorted. For
--   our current purposes that means it is an Int
type Bit = Int

-- | A Byte is a list of Bits standardized to a fixed maximum length (Bytesize)

-- | The length should be set either in or upstream of any function that uses
--   Bytes
type Byte = [Bit]

-- | An Address is a index number pointing to data stored in Memory
type Address = Int

-- | A TopBit contains a copy of the last (i.e. highest) Bit in a Byte or
--   Tensor
type TopBit = Bit

-- | A Record is an element in a Tensor or Metatensor's Register
--   containing an Address pointer and a TopBit value

-- | A Record's Address is an index number pointing to a Byte or Tensor in
--   the Tensor/Metatensor's Memory

-- | A Record's TopBit is a copy of the last (i.e. highest) Bit in the Byte or
--   Tensor that the Record references
type Record = (Address, TopBit)

-- | A Register is a list of Records allowing for easy access to data in a
--   Tensor or Metatensor's Memory
type Register = [Record]

-- | We use a Sortable type sort between Ints and Records

-- | In the future this may be expanded to include other data types and allow
--   for sorting other types of besides Ints
data Sortable
  = SortInt [Int]
  | SortRec [Record]
  deriving (Show, Eq, Ord)

fromSortInt :: Sortable -> [Int]
fromSortInt (SortInt ints) = ints
fromSortInt (SortRec _) = error "This is for sorting Integers - you gave me Records"

fromSortRec :: Sortable -> [Record]
fromSortRec (SortRec recs) = recs
fromSortRec (SortInt _) = error "This is for sorting Records - you gave me Integers"

type SortAlg = Sortable -> Sortable

type SupersortProps = (SortAlg, SortAlg, SortAlg, SupersortStrat)

type SupersortStrat = (Sortable, Sortable, Sortable) -> Sortable

-- | A Memory contains the data to be sorted, either in the form of Bytes or
--   Tensors
data Memory
  = ByteMem [Byte]
  | TensorMem [Tensor]
  deriving (Show, Eq, Ord)

-- | A Tensor is a Metatensor that only contains Bytes in its memory
-- | The Memory is a list of the Bytes or Tensors that the Tensor
--   contains.

-- | The Register is a list of Records referencing the top Bits in Memory
type Tensor = (Register, Memory)

-- | A Tensorstack is a top-level Tensor. In the final stages of Tensort, the
--   number of Tensorstacks will equal the bytesize, but before that time there
--   are expected to be many more Tensorstacks
type Tensorstack = Tensor
