-- | Module for converting raw input data into SBytes.
--
--   TODO: See if we can clean up the type conversion here.
module Data.Tensort.Utils.Convert (rawToBytes) where

import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types
  ( Bit,
    Byte,
    Record,
    SBytes (SBytesBit, SBytesRec),
    Sortable (..),
    TensortProps (..),
    fromSortBit,
    fromSortRec,
  )

-- | Convert a list of Bits to a list of Bytes of given bytesize, sorting
--   each Byte with the given subalgorithm.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
--   >>> rawBitsToBytes (mkTsProps 4 bubblesort) [5,1,3,7,8,2,4,6]
--   [[2,4,6,8],[1,3,5,7]]
rawToBytes :: TensortProps -> Sortable -> SBytes
rawToBytes tsProps (SortBit xs) = SBytesBit $ rawBitsToBytes tsProps xs
rawToBytes tsProps (SortRec xs) = SBytesRec $ rawRecsToBytes tsProps xs

rawBitsToBytes :: TensortProps -> [Bit] -> [Byte]
rawBitsToBytes tsProps bits = foldr acc [] bytes
  where
    bytes = splitEvery (bytesize tsProps) bits
    acc :: [Bit] -> [Byte] -> [Byte]
    acc byte bytesSorted =
      bytesSorted ++ [byteSorted]
      where
        byteSorted = fromSortBit . subAlgorithm tsProps $ SortBit byte

rawRecsToBytes :: TensortProps -> [Record] -> [[Record]]
rawRecsToBytes tsProps recs = foldr acc [] rbytes
  where
    rbytes = splitEvery (bytesize tsProps) recs
    acc :: [Record] -> [[Record]] -> [[Record]]
    acc rbyte rbytesSorted =
      rbytesSorted ++ [rbyteSorted]
      where
        rbyteSorted = fromSortRec . subAlgorithm tsProps $ SortRec rbyte
