module Data.Tensort.Utils.Convert (rawBitsToBytes) where

import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types (Bit, Byte, Sortable (..), TensortProps (..), fromSortBit)

-- | Convert a list of Bits to a list of Bytes of given bytesize, sorting
--   each byte with the given subalgorithm.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
--   >>> rawBitsToBytes (mkTsProps 4 bubblesort) [5,1,3,7,8,2,4,6]
--   [[2,4,6,8],[1,3,5,7]]
rawBitsToBytes :: TensortProps -> [Bit] -> [Byte]
rawBitsToBytes tsProps bits = foldr acc [] (splitEvery (bytesize tsProps) bits)
  where
    acc :: [Bit] -> [Byte] -> [Byte]
    acc byte bytes = bytes ++ [fromSortBit (subAlgorithm tsProps (SortBit byte))]
