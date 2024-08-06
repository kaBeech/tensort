module Data.Tensort.Utils.Convert (rawBitsToBytes) where

import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types (Bit, Byte, Sortable (..), TensortProps (..), fromSortBit)

-- | Convert a list of Bits to a list of Bytes of given bytesize, bubblesorting
--   each byte.

-- | ==== __Examples__
--   >>> rawBitsToBytes [5,1,3,7,8,2,4,6] 4
--   [[2,4,6,8],[1,3,5,7]]
rawBitsToBytes :: [Bit] -> TensortProps -> [Byte]
rawBitsToBytes bits tsProps = foldr acc [] (splitEvery (bytesize tsProps) bits)
  where
    acc :: [Bit] -> [Byte] -> [Byte]
    acc byte bytes = bytes ++ [fromSortBit (subAlgorithm tsProps (SortBit byte))]
