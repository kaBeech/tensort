--   TODO: See if we can clean up the type conversion here.
module Data.Tensort.Utils.Convert (rawBitsToBytes) where

import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types
  ( Bit,
    Byte,
    TensortProps (..),
  )

-- | Convert a list of Bits to a list of Bytes of given bytesize, sorting
--   each Byte with the given subalgorithm.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
--   >>> rawBitsToBytes (mkTsProps 4 bubblesort) [5,1,3,7,8,2,4,6]
--   [[2,4,6,8],[1,3,5,7]]
rawBitsToBytes :: TensortProps a -> [Bit a] -> [Byte a]
rawBitsToBytes tsProps bits = foldr acc [] bytes
  where
    bytes = splitEvery (bytesize tsProps) bits
    acc byte bytesSorted =
      bytesSorted ++ [byteSorted]
      where
        byteSorted = subAlgorithm tsProps byte
