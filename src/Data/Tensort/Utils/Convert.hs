module Data.Tensort.Utils.Convert (rawBitsToBytes) where

import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types (Bit, Byte, Sortable (..), TensortProps (..), fromSortBit, WonkyState)

-- | Convert a list of Bits to a list of Bytes of given bytesize, sorting
--   each byte with the given subalgorithm.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
--   >>> rawBitsToBytes [5,1,3,7,8,2,4,6] (mkTsProps 4 bubblesort)
--   [[2,4,6,8],[1,3,5,7]]
rawBitsToBytes :: [Bit] -> TensortProps -> WonkyState -> ([Byte], WonkyState)
rawBitsToBytes bits tsProps wonkySt = foldr acc ([], wonkySt) (splitEvery (bytesize tsProps) bits)
  where
    acc :: [Bit] -> ([Byte], WonkyState) -> ([Byte], WonkyState)
    acc byte (bytes, wonkySt') = do
      let (result, wonkySt'') = (subAlgorithm tsProps) (SortBit byte) wonkySt'
      ((bytes ++ [fromSortBit result]), wonkySt'')
