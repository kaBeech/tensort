module Data.Tensort.Utils.Convert (rawToBytes) where

import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types (Bit, Byte, Record, SBytes (SBytesBit, SBytesRec), Sortable (..), TensortProps (..), WonkyState, fromSortBit, fromSortRec)

-- | Convert a list of Bits to a list of Bytes of given bytesize, sorting
--   each byte with the given subalgorithm.

-- | ==== __Examples__
--   >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
--   >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
--   >>> rawBitsToBytes (mkTsProps 4 bubblesort) [5,1,3,7,8,2,4,6]
--   [[2,4,6,8],[1,3,5,7]]
rawToBytes :: TensortProps -> WonkyState -> Sortable -> (SBytes, WonkyState)
rawToBytes tsProps wonkySt (SortBit xs) = do
  let (result, wonkySt') = rawBitsToBytes tsProps wonkySt xs
  (SBytesBit result, wonkySt')
rawToBytes tsProps wonkySt (SortRec xs) = do
  let (result, wonkySt') = rawRecsToBytes tsProps wonkySt xs
  (SBytesRec result, wonkySt')

rawBitsToBytes :: TensortProps -> WonkyState -> [Bit] -> ([Byte], WonkyState)
rawBitsToBytes tsProps wonkySt bits = foldr acc ([], wonkySt) (splitEvery (bytesize tsProps) bits)
  where
    acc :: [Bit] -> ([Byte], WonkyState) -> ([Byte], WonkyState)
    acc byte (bytes, wonkySt') = do
      let (result, wonkySt'') = subAlgorithm tsProps wonkySt' (SortBit byte)
      (bytes ++ [fromSortBit result], wonkySt'')

rawRecsToBytes :: TensortProps -> WonkyState -> [Record] -> ([[Record]], WonkyState)
rawRecsToBytes tsProps wonkySt recs = foldr acc ([], wonkySt) (splitEvery (bytesize tsProps) recs)
  where
    acc :: [Record] -> ([[Record]], WonkyState) -> ([[Record]], WonkyState)
    acc rbyte (rbytes, wonkySt') = do
      let (result, wonkySt'') = subAlgorithm tsProps wonkySt' (SortRec rbyte)
      (rbytes ++ [fromSortRec result], wonkySt'')
