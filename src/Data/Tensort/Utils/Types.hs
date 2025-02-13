{-# LANGUAGE GADTs #-}

-- | This module provides types used in the Tensort package.
module Data.Tensort.Utils.Types where

-- | TensortProps contains the Bytesize and SubAlgorithm used in a Tensort
--   algorithm.
data TensortProps = TensortProps {bytesize :: Int, subAlgorithm :: SortAlg}

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
type Tensor = (Register, Memory)

-- | A TensorStack is a top-level Tensor. In the final stages of Tensort, the
--   number of TensorStacks will be equal to (or sometimes less than) the
--   bytesize, but before that time there are expected to be many more
--   TensorStacks.
type TensorStack = Tensor

-- | We use a Sortable type to sort list of Bits and lists of Records.
type Sortable = [Bit]

-- | A sorting algorithm is a function that takes a Sortable and returns a
--   sorted Sortable.
-- type SortAlg = (Ord a) => [a] -> [a]

-- | SupersortProps consist of three sorting algorithms to adjuditcate between
--   and a SupersortStrat that does the adjudication.
-- type SupersortProps = (SortAlg, SortAlg, SortAlg, SupersortStrat)

-- | A SupersortStrat takes three Sortables and determines which of the three
--   is most likely to be in the correct order.
-- type SupersortStrat = (Sortable, Sortable, Sortable) -> Sortable

-- | Converts a Maybe into a value or throws an error if the Maybe is Nothing.
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

--------------------------------------
-- Types used for recursive Tensort --
--------------------------------------

-- | This is a `Bit` type that is used when sorting Records in a recursive
--   Tensort variant.
type BitR = Record

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants.
data SBit
  = SBitBit Bit
  | SBitRec Record
  deriving (Show, Eq, Ord)

-- | Converts an SBit into a Bit.
fromSBitBit :: SBit -> Bit
fromSBitBit (SBitBit bit) = bit
fromSBitBit (SBitRec _) =
  error
    "From fromSBitBit: This is for sorting Bits - you gave me Records"

-- | Converts an SBit into a Record.
fromSBitRec :: SBit -> Record
fromSBitRec (SBitRec record) = record
fromSBitRec (SBitBit _) =
  error
    "From fromSBitRec: This is for sorting Records - you gave me Bits"

-- | Converts a list of Bits into a Sortable.
fromSBitBits :: [SBit] -> Sortable
fromSBitBits = map fromSBitBit

-- | Converts a list of Records into a Sortable.
fromSBitRecs :: [SBit] -> Sortable
fromSBitRecs = map fromSBitRec

-- | This is a `Byte` type that is used when sorting Records in a recursive
--   Tensort variant.
type ByteR = [Record]

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants.
data SBytes
  = SBytesBit [Byte]
  | SBytesRec [ByteR]
  deriving (Show, Eq, Ord)

-- | Converts an SBytes list into a list of Bytes.
fromSBytesBit :: SBytes -> [[Bit]]
fromSBytesBit (SBytesBit bits) = bits
fromSBytesBit (SBytesRec _) =
  error
    "From fromSBytesBit: This is for sorting Bits - you gave me Records"

-- | Converts an SBytes list into a list of ByteRs.
fromSBytesRec :: SBytes -> [[Record]]
fromSBytesRec (SBytesRec recs) = recs
fromSBytesRec (SBytesBit _) =
  error
    "From fromSBytesRec: This is for sorting Records - you gave me Bits"

-- | This is a `TopBit` type that is used when sorting Records in a recursive
--   Tensort variant.
type TopBitR = Record

-- | This is a `Record` type that is used when sorting Records in a recursive
--   Tensort variant.
type RecordR = (Address, TopBitR)

-- | This is a conversion type that allows for sorting both Records and Bits.
--   It is useful in recursive Tensort variants.
data SRecord
  = SRecordBit Record
  | SRecordRec RecordR
  deriving (Show, Eq, Ord)

-- | Converts an SRecord into a Record.
fromSRecordBit :: SRecord -> Record
fromSRecordBit (SRecordBit record) = record
fromSRecordBit (SRecordRec _) =
  error
    "From fromSRecordBit: This is for sorting Records - you gave me Bits"

-- | Converts an SRecord into a RecordR.
fromSRecordRec :: SRecord -> RecordR
fromSRecordRec (SRecordRec record) = record
fromSRecordRec (SRecordBit _) =
  error
    "From fromSRecordRec: This is for sorting Bits - you gave me Records"

-- | This is a conversion type that allows for sorting both Records and Bits.
--   It is useful in recursive Tensort variants.
data SRecords
  = SRecordsBit [Record]
  | SRecordsRec [RecordR]
  deriving (Show, Eq, Ord)

-- | Converts an SRecords list into a list of Records.
fromSRecordsBit :: SRecords -> [Record]
fromSRecordsBit (SRecordsBit records) = records
fromSRecordsBit (SRecordsRec _) =
  error
    "From fromSRecordsBit: This is for sorting Records - you gave me Bits"

-- | Converts an SRecords list into a list of RecordRs.
fromSRecordsRec :: SRecords -> [RecordR]
fromSRecordsRec (SRecordsRec records) = records
fromSRecordsRec (SRecordsBit _) =
  error
    "From fromSRecordsRec: This is for sorting Bits - you gave me Records"

-- | Converts a list of SRecords into a list of Records.
fromSRecordArrayBit :: [SRecord] -> [Record]
fromSRecordArrayBit = map fromSRecordBit

-- | Converts a list of SRecords into a list of RecordRs.
fromSRecordArrayRec :: [SRecord] -> [RecordR]
fromSRecordArrayRec = map fromSRecordRec

-- | This is a `Register` type that is used when sorting Records in a recursive
--   Tensort variant.
type RegisterR = [RecordR]

-- | This is a `Memory` type that is used when sorting Records in a recursive
--   Tensort variant.
data MemoryR
  = ByteMemR [ByteR]
  | TensorMemR [TensorR]
  deriving (Show, Eq, Ord)

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants.
data SMemory
  = SMemoryBit Memory
  | SMemoryRec MemoryR
  deriving (Show, Eq, Ord)

-- | Converts an SMemory to a Memory.
fromSMemoryBit :: SMemory -> Memory
fromSMemoryBit (SMemoryBit memory) = memory
fromSMemoryBit (SMemoryRec _) =
  error
    "From fromSTensorsRec: This is for sorting Bits - you gave me Records"

-- | Converts an SMemory to a MemoryR.
fromSMemoryRec :: SMemory -> MemoryR
fromSMemoryRec (SMemoryRec memory) = memory
fromSMemoryRec (SMemoryBit _) =
  error
    "From fromSMemoryRec: This is for sorting Records - you gave me Bits"

-- | This is a `Tensor` type that is used when sorting Records in a recursive
--   Tensort variant.
type TensorR = (RegisterR, MemoryR)

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants.
data STensor
  = STensorBit Tensor
  | STensorRec TensorR
  deriving (Show, Eq, Ord)

-- | Converts an STensor into a Tensor.
fromSTensorBit :: STensor -> Tensor
fromSTensorBit (STensorBit tensor) = tensor
fromSTensorBit (STensorRec _) =
  error
    "From fromSTensorBit: This is for sorting Tensors - you gave me Records"

-- | Converts an STensor into a TensorR.
fromSTensorRec :: STensor -> TensorR
fromSTensorRec (STensorRec tensor) = tensor
fromSTensorRec (STensorBit _) =
  error
    "From fromSTensorRec: This is for sorting Records - you gave me Tensors"

-- | This is a conversion type that allows for sorting both Bits and Records.
--   It is useful in recursive Tensort variants.
data STensors
  = STensorsBit [Tensor]
  | STensorsRec [TensorR]
  deriving (Show, Eq, Ord)

-- | Converts an STensors list into a list of Tensors.
fromSTensorsBit :: STensors -> [Tensor]
fromSTensorsBit (STensorsBit tensors) = tensors
fromSTensorsBit (STensorsRec _) =
  error
    "From fromSTensorsBit: This is for sorting Tensors - you gave me Records"

-- | Converts an STensors list into a list of TensorRs.
fromSTensorsRec :: STensors -> [TensorR]
fromSTensorsRec (STensorsRec tensors) = tensors
fromSTensorsRec (STensorsBit _) =
  error
    "From fromSTensorsRec: This is for sorting Records - you gave me Tensors"

-- | This is a `TensorStack` type that is used when sorting Records in a
--   recursive Tensort variant.
type TensorStackR = TensorR

-- | This is a conversion type that allows for sorting both Tensors and
--   Records. It is useful in recursive Tensort variants.
type STensorStack = STensor

-- | This is a conversion type that allows for sorting both Tensors and
--   Records. It is useful in recursive Tensort variants.
type STensorStacks = STensors
