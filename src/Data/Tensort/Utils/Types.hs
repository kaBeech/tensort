{-# LANGUAGE GADTs #-}

-- | This module provides types used in the Tensort package
--
--   Since these packages are only for sorting Ints currently, every data
--   type is a structure of Ints
module Data.Tensort.Utils.Types where

import System.Random (StdGen)

-- | TensortProps contains the Bytesize and SubAlgorithm used in a Tensort
--   algorithm
data TensortProps = TensortProps {bytesize :: Int, subAlgorithm :: SortAlg}

data WonkyState = WonkyState {wonkyChance :: Int, stuckChance :: Int, previousAnswer :: Int, stdGen :: StdGen}

--   All the data types used in the Tensort and Tensort algorithms are
--   defined here. Since these packages are only for sorting Ints currently,
--   every data type is a structure of Ints

-- | A Bit is a single element of the list to be sorted. For
--   our current purposes that means it is an Int

--   The definition of a Bit may be expanded in the future to include any Ord
type Bit = Int

-- | This is a `Bit` type that is used when sorting Records in a recursive
--   Tensort variant
type BitR = Record

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants
data SBit
  = SBitBit Bit
  | SBitRec Record
  deriving (Show, Eq, Ord)

-- | Converts an SBit to a Bit
fromSBitBit :: SBit -> Bit
fromSBitBit (SBitBit bit) = bit
fromSBitBit (SBitRec _) = error "From fromSBitBit: This is for sorting Bits - you gave me Records"

-- | Converts an SBit to a Record
fromSBitRec :: SBit -> Record
fromSBitRec (SBitRec record) = record
fromSBitRec (SBitBit _) = error "From fromSBitRec: This is for sorting Records - you gave me Bits"

-- | A Byte is a list of Bits standardized to a fixed maximum length (Bytesize)

-- | The length should be set either in or upstream of any function that uses
--   Bytes
type Byte = [Bit]

-- | This is a `Byte` type that is used when sorting Records in a recursive
--   Tensort variant
type ByteR = [Record]

-- | An Address is a index number pointing to data stored in Memory
type Address = Int

-- | A TopBit contains a copy of the last (i.e. highest) Bit in a Byte or
--   Tensor
type TopBit = Bit

-- | This is a `TopBit` type that is used when sorting Records in a recursive
--   Tensort variant
type TopBitR = Record

-- | A Record is an element in a Tensor's Register
--   containing an Address pointer and a TopBit value

-- | A Record's Address is an index number pointing to a Byte or Tensor in
--   the Tensor's Memory

-- | A Record's TopBit is a copy of the last (i.e. highest) Bit in the Byte or
--   Tensor that the Record references
type Record = (Address, TopBit)

-- | This is a `Record` type that is used when sorting Records in a recursive
--   Tensort variant
type RecordR = (Address, TopBitR)

-- | This is a conversion type that allows for sorting both Records and Bits.
--   It is useful in recursive Tensort variants
data SRecord
  = SRecordBit Record
  | SRecordRec RecordR
  deriving (Show, Eq, Ord)

-- | Converts an SRecord to a Record
fromSRecordBit :: SRecord -> Record
fromSRecordBit (SRecordBit record) = record
fromSRecordBit (SRecordRec _) = error "From fromSRecordBit: This is for sorting Records - you gave me Bits"

-- | Converts an SRecord to a RecordR
fromSRecordRec :: SRecord -> RecordR
fromSRecordRec (SRecordRec record) = record
fromSRecordRec (SRecordBit _) = error "From fromSRecordRec: This is for sorting Bits - you gave me Records"

-- | This is a conversion type that allows for sorting both Records and Bits.
--   It is useful in recursive Tensort variants
data SRecords
  = SRecordsBit [Record]
  | SRecordsRec [RecordR]
  deriving (Show, Eq, Ord)

-- | Converts an SRecords to a list of Records
fromSRecordsBit :: SRecords -> [Record]
fromSRecordsBit (SRecordsBit records) = records
fromSRecordsBit (SRecordsRec _) = error "From fromSRecordsBit: This is for sorting Records - you gave me Bits"

-- | Converts an SRecords to a list of RecordRs
fromSRecordsRec :: SRecords -> [RecordR]
fromSRecordsRec (SRecordsRec records) = records
fromSRecordsRec (SRecordsBit _) = error "From fromSRecordsRec: This is for sorting Bits - you gave me Records"

-- | Converts a list of SRecords to a list of Records
fromSRecordArrayBit :: [SRecord] -> [Record]
fromSRecordArrayBit = map fromSRecordBit

-- | Converts a list of SRecords to a list of RecordRs
fromSRecordArrayRec :: [SRecord] -> [RecordR]
fromSRecordArrayRec = map fromSRecordRec

-- | A Register is a list of Records allowing for easy access to data in a
--   Tensor's Memory
type Register = [Record]

-- | This is a `Register` type that is used when sorting Records in a recursive
--   Tensort variant
type RegisterR = [RecordR]

-- | We use a Sortable type to sort Bits and Records
data Sortable
  = SortBit [Bit]
  | SortRec [Record]
  deriving (Show, Eq, Ord)

-- | Converts a Sortable list to a list of Bits
fromSortBit :: Sortable -> [Bit]
fromSortBit (SortBit bits) = bits
fromSortBit (SortRec _) = error "From fromSortBit: This is for sorting Bits - you gave me Records"

-- | Converts a Sortable list to a list of Records
fromSortRec :: Sortable -> [Record]
fromSortRec (SortRec recs) = recs
fromSortRec (SortBit _) = error "From fromSortRec: This is for sorting Records - you gave me Bits"

-- | Converts a list of Bits to a Sortable
fromSBitBits :: [SBit] -> Sortable
fromSBitBits = SortBit . map fromSBitBit

-- | Converts a list of Records to a Sortable
fromSBitRecs :: [SBit] -> Sortable
fromSBitRecs = SortRec . map fromSBitRec

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants
data SBytes
  = SBytesBit [Byte]
  | SBytesRec [ByteR]
  deriving (Show, Eq, Ord)

-- | Converts an SBytes list to a list of Bytes
fromSBytesBit :: SBytes -> [[Bit]]
fromSBytesBit (SBytesBit bits) = bits
fromSBytesBit (SBytesRec _) = error "From fromSBytesBit: This is for sorting Bits - you gave me Records"

-- | Converts an SBytes list to a list of ByteRs
fromSBytesRec :: SBytes -> [[Record]]
fromSBytesRec (SBytesRec recs) = recs
fromSBytesRec (SBytesBit _) = error "From fromSBytesRec: This is for sorting Records - you gave me Bits"

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants
data STensor
  = STensorBit Tensor
  | STensorRec TensorR
  deriving (Show, Eq, Ord)

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants
data STensors
  = STensorsBit [Tensor]
  | STensorsRec [TensorR]
  deriving (Show, Eq, Ord)

-- | Converts an STensor to a Tensor
fromSTensorBit :: STensor -> Tensor
fromSTensorBit (STensorBit tensor) = tensor
fromSTensorBit (STensorRec _) =
  error
    "From fromSTensorBit: This is for sorting Tensors - you gave me Records"

-- | Converts an STensor to a TensorR
fromSTensorRec :: STensor -> TensorR
fromSTensorRec (STensorRec tensor) = tensor
fromSTensorRec (STensorBit _) =
  error
    "From fromSTensorRec: This is for sorting Records - you gave me Tensors"

-- | Converts an STensors list to a list of Tensors
fromSTensorsBit :: STensors -> [Tensor]
fromSTensorsBit (STensorsBit tensors) = tensors
fromSTensorsBit (STensorsRec _) =
  error
    "From fromSTensorsBit: This is for sorting Tensors - you gave me Records"

-- | Converts an STensors list to a list of TensorRs
fromSTensorsRec :: STensors -> [TensorR]
fromSTensorsRec (STensorsRec tensors) = tensors
fromSTensorsRec (STensorsBit _) =
  error
    "From fromSTensorsRec: This is for sorting Records - you gave me Tensors"

-- | A sorting algorithm is a function that takes a Sortable and returns a
--   sorted Sortable
type SortAlg = WonkyState -> Sortable -> (Sortable, WonkyState)

-- | SupersortProps consist of three sorting algorithms to adjuditcate between
--   and a SupersortStrat that does the adjudication
type SupersortProps = (SortAlg, SortAlg, SortAlg, SupersortStrat)

-- | A SupersortStrat takes three Sortables and determines which of the three
--   is most likely to be in the correct order
type SupersortStrat = (Sortable, Sortable, Sortable) -> Sortable

-- | A Memory contains the data to be sorted, either in the form of Bytes or
--   Tensors.
data Memory
  = ByteMem [Byte]
  | TensorMem [Tensor]
  deriving (Show, Eq, Ord)

-- | This is a `Memory` type that is used when sorting Records in a recursive
--   Tensort variant
data MemoryR
  = ByteMemR [ByteR]
  | TensorMemR [TensorR]
  deriving (Show, Eq, Ord)

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants
data SMemory
  = SMemoryBit Memory
  | SMemoryRec MemoryR
  deriving (Show, Eq, Ord)

-- | Converts an SMemory to a Memory
fromSMemoryBit :: SMemory -> Memory
fromSMemoryBit (SMemoryBit memory) = memory
fromSMemoryBit (SMemoryRec _) = error "From fromSTensorsRec: This is for sorting Bits - you gave me Records"

-- | Converts an SMemory to a MemoryR
fromSMemoryRec :: SMemory -> MemoryR
fromSMemoryRec (SMemoryRec memory) = memory
fromSMemoryRec (SMemoryBit _) = error "From fromSMemoryRec: This is for sorting Records - you gave me Bits"

-- | A Tensor contains data to be sorted in a structure allowing for
--   easy access. It consists of a Register and its Memory.

-- | The Memory is a list of the Bytes or other Tensors that this Tensor
--   contains.

-- | The Register is a list of Records referencing the top Bits in Memory.
type Tensor = (Register, Memory)

-- | This is a `Tensor` type that is used when sorting Records in a recursive
--   Tensort variant
type TensorR = (RegisterR, MemoryR)

-- | A TensorStack is a top-level Tensor. In the final stages of Tensort, the
--   number of TensorStacks will be equal to (or sometimes less than) the
--   bytesize, but before that time there are expected to be many more
--   TensorStacks.
type TensorStack = Tensor

-- | This is a `TensorStack` type that is used when sorting Records in a recursive
--   Tensort variant
type TensorStackR = TensorR

-- | This is a conversion type that allows for sorting both Tensors and Records.
--   It is useful in recursive Tensort variants
type STensorStack = STensor

-- | This is a conversion type that allows for sorting both Tensors and Records.
--   It is useful in recursive Tensort variants
type STensorStacks = STensors

-- | Convers a Maybe into a value or throws an error if the Maybe is Nothing
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
