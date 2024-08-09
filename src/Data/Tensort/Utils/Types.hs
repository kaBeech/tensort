{-# LANGUAGE GADTs #-}

module Data.Tensort.Utils.Types where

data TensortProps = TensortProps {bytesize :: Int, subAlgorithm :: SortAlg}

--   All the data types used in the Tensort and Tensort algorithms are
--   defined here. Since these packages are only for sorting Ints currently,
--   every data type is a structure of Ints

-- | A Bit is a single element of the list to be sorted. For
--   our current purposes that means it is an Int

-- | NOTE: To Self: at this point it's likely simple enough to refactor this
--   to sort any Ord, not just Ints. Consider using the `Bit` type synonym
--   in the code, then changing this to alias `Bit` to `Ord` or `a`
type Bit = Int

type BitR = Record

data SBit
  = SBitBit Bit
  | SBitRec Record
  deriving (Show, Eq, Ord)

fromSBitBit :: SBit -> Bit
fromSBitBit (SBitBit bit) = bit
fromSBitBit (SBitRec _) = error "This is for sorting Bits - you gave me Records"

fromSBitRec :: SBit -> Record
fromSBitRec (SBitRec record) = record
fromSBitRec (SBitBit _) = error "This is for sorting Records - you gave me Bits"

-- | A Byte is a list of Bits standardized to a fixed maximum length (Bytesize)

-- | The length should be set either in or upstream of any function that uses
--   Bytes
type Byte = [Bit]

type ByteR = [Record]

-- | An Address is a index number pointing to data stored in Memory
type Address = Int

-- | A TopBit contains a copy of the last (i.e. highest) Bit in a Byte or
--   Tensor
type TopBit = Bit

type TopBitR = Record

-- | A Record is an element in a Tensor's Register
--   containing an Address pointer and a TopBit value

-- | A Record's Address is an index number pointing to a Byte or Tensor in
--   the Tensor's Memory

-- | A Record's TopBit is a copy of the last (i.e. highest) Bit in the Byte or
--   Tensor that the Record references
type Record = (Address, TopBit)

type RecordR = (Address, TopBitR)

data SRecord
  = SRecordBit Record
  | SRecordRec RecordR
  deriving (Show, Eq, Ord)

fromSRecordBit :: SRecord -> Record
fromSRecordBit (SRecordBit record) = record
fromSRecordBit (SRecordRec _) = error "This is for sorting Records - you gave me Bits"

fromSRecordRec :: SRecord -> RecordR
fromSRecordRec (SRecordRec record) = record
fromSRecordRec (SRecordBit _) = error "This is for sorting Bits - you gave me Records"

data SRecords
  = SRecordsBit [Record]
  | SRecordsRec [RecordR]
  deriving (Show, Eq, Ord)

fromSRecordsBit :: SRecords -> [Record]
fromSRecordsBit (SRecordsBit records) = records
fromSRecordsBit (SRecordsRec _) = error "This is for sorting Records - you gave me Bits"

fromSRecordsRec :: SRecords -> [RecordR]
fromSRecordsRec (SRecordsRec records) = records
fromSRecordsRec (SRecordsBit _) = error "This is for sorting Bits - you gave me Records"

fromSRecordArrayBit :: [SRecord] -> [Record]
fromSRecordArrayBit = map fromSRecordBit

fromSRecordArrayRec :: [SRecord] -> [RecordR]
fromSRecordArrayRec = map fromSRecordRec

-- | A Register is a list of Records allowing for easy access to data in a
--   Tensor's Memory
type Register = [Record]

type RegisterR = [RecordR]

-- | We use a Sortable type to sort Bits and Records
data Sortable
  = SortBit [Bit]
  | SortRec [Record]
  deriving (Show, Eq, Ord)

fromSortBit :: Sortable -> [Bit]
fromSortBit (SortBit bits) = bits
fromSortBit (SortRec _) = error "This is for sorting Bits - you gave me Records"

fromSortRec :: Sortable -> [Record]
fromSortRec (SortRec recs) = recs
fromSortRec (SortBit _) = error "This is for sorting Records - you gave me Bits"

data SBytes
  = SBytesBit [Byte]
  | SBytesRec [ByteR]
  deriving (Show, Eq, Ord)

fromSBytesBit :: SBytes -> [[Bit]]
fromSBytesBit (SBytesBit bits) = bits
fromSBytesBit (SBytesRec _) = error "This is for sorting Bits - you gave me Records"

fromSBytesRec :: SBytes -> [[Record]]
fromSBytesRec (SBytesRec recs) = recs
fromSBytesRec (SBytesBit _) = error "This is for sorting Records - you gave me Bits"

data STensor
  = STensorBit Tensor
  | STensorRec TensorR
  deriving (Show, Eq, Ord)

data STensors
  = STensorsBit [Tensor]
  | STensorsRec [TensorR]
  deriving (Show, Eq, Ord)

fromSTensorBit :: STensor -> Tensor
fromSTensorBit (STensorBit tensor) = tensor
fromSTensorBit (STensorRec _) = error "This is for sorting Tensors - you gave me Records"

fromSTensorRec :: STensor -> TensorR
fromSTensorRec (STensorRec tensor) = tensor
fromSTensorRec (STensorBit _) = error "This is for sorting Records - you gave me Tensors"

fromSTensorsBit :: STensors -> [Tensor]
fromSTensorsBit (STensorsBit tensors) = tensors
fromSTensorsBit (STensorsRec _) = error "This is for sorting Tensors - you gave me Records"

fromSTensorsRec :: STensors -> [TensorR]
fromSTensorsRec (STensorsRec tensors) = tensors
fromSTensorsRec (STensorsBit _) = error "This is for sorting Records - you gave me Tensors"

type SortAlg = Sortable -> Sortable

type SupersortProps = (SortAlg, SortAlg, SortAlg, SupersortStrat)

type SupersortStrat = (Sortable, Sortable, Sortable) -> Sortable

-- | A Memory contains the data to be sorted, either in the form of Bytes or
--   Tensors.
data Memory
  = ByteMem [Byte]
  | TensorMem [Tensor]
  deriving (Show, Eq, Ord)

data MemoryR
  = ByteMemR [ByteR]
  | TensorMemR [TensorR]
  deriving (Show, Eq, Ord)

data SMemory
  = SMemoryBit Memory
  | SMemoryRec MemoryR
  deriving (Show, Eq, Ord)

fromSMemoryBit :: SMemory -> Memory
fromSMemoryBit (SMemoryBit memory) = memory
fromSMemoryBit (SMemoryRec _) = error "This is for sorting Bits - you gave me Records"

fromSMemoryRec :: SMemory -> MemoryR
fromSMemoryRec (SMemoryRec memory) = memory
fromSMemoryRec (SMemoryBit _) = error "This is for sorting Records - you gave me Bits"

-- | A Tensor contains data to be sorted in a structure allowing for
--   easy access. It consists of a Register and its Memory.

-- | The Memory is a list of the Bytes or other Tensors that this Tensor
--   contains.

-- | The Register is a list of Records referencing the top Bits in Memory.
type Tensor = (Register, Memory)

type TensorR = (RegisterR, MemoryR)

-- | A TensorStack is a top-level Tensor. In the final stages of Tensort, the
--   number of TensorStacks will equal the bytesize, but before that time there
--   are expected to be many more TensorStacks.
type TensorStack = Tensor

type TensorStackR = TensorR

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
