{-# LANGUAGE GADTs #-}

module Data.Robustsort.Utils.Types where

--   All the data types used in the Bytesort and Robustsort algorithms are
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
--   Metabyte
type TopBit = Bit

-- | A Record is an element in a Bytestore or Metabytestore's Register
--   containing an Address pointer and a TopBit value

-- | A Record's Address is an index number pointing to a Byte or Metabyte in
--   the Bytestore/Metabytestore's Memory

-- | A Record's TopBit is a copy of the last (i.e. highest) Bit in the Byte or
--   Metabyte that the Record references
type Record = (Address, TopBit)

-- | A Register is a list of Records allowing for easy access to data in a
--   Bytestore or Metabytestore's Memory
type Register = [Record]

-- | A Memory contains the data to be sorted, either in the form of Bytes or
--   Metabytes
data Memory
  = SmallMemory [Byte]
  | BigMemory [Bytestore]
  deriving (Show, Eq, Ord)

-- | A Bytestore is a Metabytestore that only contains Bytes in its memory
-- | The Memory is a list of the Bytes or Metabytes that the Metabytestore
--   contains.

-- | The Register is a list of Records referencing the top Bits in Memory
type Bytestore = (Register, Memory)

-- | A Metabyte is either a Bytestore or a Metabytestore

-- | I know this might sound confusing, but in a recursive algorithm like this
--   it's helpful to have different names for the same type of data depending
--   on how it's being used, while still being able to use the same data in
--   multiple contexts
-- data Metabyte
--   = Metabyte Bytestore
--   | BigMetabyte Metabytestore
--   deriving (Show)

-- | A Metabytestore contains data to be sorted in a structure allowing for
--   easy access

-- | The Memory is a list of the Bytes or Metabytes that the Metabytestore
--   contains

-- | The Register is a list of Records referencing the top Bits in Memory
-- type Metabytestore = (Register, Memory)

-- | A Bytestack is a top-level Metabyte. In the final stages of Bytesort, the
--   number of Bytestacks will equal the bytesize, but before that time there
--   are expected to be many more Bytestacks
type Bytestack = Bytestore
