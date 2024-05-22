module Data.Robustsort.Utils.Types where

type Bit = Int

type Byte = [Bit]

type Address = Int

type TopBit = Bit

type Reference = (Address, TopBit)

type Register = [Reference]

type Bytes = [Byte]

type Metabytes = [Metabyte]

data Memory = Bytes | Metabytes

type Bytestore = (Register, Memory)

data Metabyte = Bytestore | Metabytestore

type Metabytestore = (Register, Memory)

type Bytestack = Metabyte
