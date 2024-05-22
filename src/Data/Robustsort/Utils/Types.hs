module Data.Robustsort.Utils.Types where

type Bit = Int

type Byte = [Bit]

type Address = Int

type TopBit = Bit

type Reference = (Address, TopBit)

type Register = [Reference]

data Memory
  = Memory [Byte]
  | BigMemory [Metabyte]
  deriving (Show)

type Bytestore = (Register, Memory)

data Metabyte
  = Metabyte Bytestore
  | BigMetabyte Metabytestore
  deriving (Show)

type Metabytestore = (Register, Memory)

type Bytestack = Metabyte
