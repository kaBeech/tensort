-- module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestacksFromBytes, getSortedArrayFromBytestacks, reduceBytestacks) where
module Data.Robustsort.Utils.Bytes (convertRawBitsToBytes, getBytestoreFromBytes, getBytestacksFromBytes, reduceBytestacks, reduceBytestacksSinglePass, createBytestack, getRegisterFromMetabytes, getTopBitFromBytestack) where

import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort, bubblesortRecords)
import Data.Robustsort.Utils.Split (splitEvery)
import Data.Robustsort.Utils.Types (Byte, Bytestack, Bytestore, Memory (..), Record)

-- | Convert a list of Bits to a list of Bytes of given bytesize, bubblesorting
--   each byte.

-- | ==== __Examples__
--   >>> convertRawBitsToBytes [5,1,3,7,8,2,4,6] 4
--   [[2,4,6,8],[1,3,5,7]]
convertRawBitsToBytes :: [Int] -> Int -> [Byte]
convertRawBitsToBytes bits bytesize = foldr acc [] (splitEvery bytesize bits)
  where
    acc :: [Int] -> [Byte] -> [Byte]
    acc byte bytes = bytes ++ [bubblesort byte]

-- | Convert a list of Bytes to a list of Bytestacks.

-- | This is accomplished by making a Bytestore for each Byte, converting that
--   Bytestore into a Bytestack (these are equivalent terms - see type
--   definitions for more info) and collating the Bytestacks into a list

-- | ==== __Examples__
--  >>> getBytestacksFromBytes [[2,4],[6,8],[1,3],[5,7]] 2
--  [([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(0,4),(1,8)],Memory [[2,4],[6,8]])]
getBytestacksFromBytes :: [Byte] -> Int -> [Bytestack]
getBytestacksFromBytes bytes bytesize = foldr acc [] (splitEvery bytesize bytes)
  where
    acc :: [Byte] -> [Bytestack] -> [Bytestack]
    acc byte bytestacks = bytestacks ++ [getBytestoreFromBytes byte]

-- | Convert a list of Bytes to a Bytestore

-- | We do this by loading the list of Bytes into the new Bytestore's Memory
--   and adding a sorted Register containing References to each Byte in Memory

-- | Each Record contains an Address pointing to the index of the referenced
--   Byte and a TopBit containing the value of the last (i.e. highest) Bit in
--   the referenced Byte

-- | The Register is bubblesorted by the TopBits of each Record

-- | ==== __Examples__
--  >>> getBytestoreFromBytes [[2,4,6,8],[1,3,5,7]]
--  ([(1,7),(0,8)],Memory [[2,4,6,8],[1,3,5,7]])
getBytestoreFromBytes :: [Byte] -> Bytestore
getBytestoreFromBytes bytes = do
  let register = acc bytes [] 0
  let register' = bubblesortRecords register
  (register', Memory bytes)
  where
    acc :: [Byte] -> [Record] -> Int -> [Record]
    acc [] register _ = register
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)

-- | Take a list of Bytestacks (Metabytes) and group them together in new
--   Bytestacks, each containing bytesize number of Metabytes (former
--   Bytestacks), until the number of Bytestacks is equal to the bytesize

-- | The Registers of the new Bytestacks are bubblesorted, as usual

-- | ==== __Examples__
-- >>> reduceBytestacks [([(0, 33), (1, 38)], Memory [[31, 33], [35, 38]]), ([(0, 34), (1, 37)], Memory [[32, 14], [36, 37]]), ([(0, 23), (1, 27)], Memory [[21, 23], [25, 27]]), ([(0, 24), (1, 28)], Memory [[22, 24], [26, 28]]),([(0,13),(1,18)],Memory [[11,13],[15,18]]),([(0,14),(1,17)],Memory [[12,14],[16,17]]),([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(0,4),(1,8)],Memory [[2,4],[6,8]])] 2
-- [([(0,28),(1,38)],BigMemory [([(0,27),(1,28)],BigMemory [([(0,23),(1,27)],Memory [[21,23],[25,27]]),([(0,24),(1,28)],Memory [[22,24],[26,28]])]),([(1,37),(0,38)],BigMemory [([(0,33),(1,38)],Memory [[31,33],[35,38]]),([(0,34),(1,37)],Memory [[32,14],[36,37]])])]),([(0,8),(1,18)],BigMemory [([(0,7),(1,8)],BigMemory [([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(0,4),(1,8)],Memory [[2,4],[6,8]])]),([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],Memory [[11,13],[15,18]]),([(0,14),(1,17)],Memory [[12,14],[16,17]])])])]
reduceBytestacks :: [Bytestack] -> Int -> [Bytestack]
reduceBytestacks bytestacks bytesize = do
  let newBytestacks = reduceBytestacksSinglePass bytestacks bytesize
  if length newBytestacks <= bytesize
    then newBytestacks
    else reduceBytestacks newBytestacks bytesize

-- | Take a list of Bytestacks (Metabytes) and group them together in new
--   Bytestacks each containing bytesize number of Metabytes (former Bytestacks)

-- | The Registers of the new Bytestacks are bubblesorted, as usual

-- | ==== __Examples__
-- >>> reduceBytestacks [([(0,13),(1,18)],Memory [[11,13],[15,18]]),([(0,14),(1,17)],Memory [[12,14],[16,17]]),([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(0,4),(1,8)],Memory [[2,4],[6,8]])] 2
-- [([(0,7),(1,8)],BigMemory [([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(0,4),(1,8)],Memory [[2,4],[6,8]])]),([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],Memory [[11,13],[15,18]]),([(0,14),(1,17)],Memory [[12,14],[16,17]])])]
reduceBytestacksSinglePass :: [Bytestack] -> Int -> [Bytestack]
reduceBytestacksSinglePass bytestacks bytesize = foldr acc [] (splitEvery bytesize bytestacks)
  where
    acc :: [Bytestack] -> [Bytestack] -> [Bytestack]
    acc bytestack newBytestacks = newBytestacks ++ [createBytestack bytestack]

-- | Create a Bytestack with the collated and bubblesorted References from the
--   Metabytes as the Register and the original Metabytes as the data

-- | ==== __Examples__
-- >>> createBytestack [([(0,13),(1,18)],Memory [[11,13],[15,18]]),([(1,14),(0,17)],Memory [[16,17],[12,14]])]
-- ([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],Memory [[11,13],[15,18]]),([(1,14),(0,17)],Memory [[16,17],[12,14]])])
createBytestack :: [Bytestore] -> Bytestack
createBytestack metabytes = (bubblesortRecords (getRegisterFromMetabytes metabytes), BigMemory metabytes)

-- | For each Bytestore, produces a Record by combining the top bit of the
--  Bytestore with an index value for its Address

-- | Note that this output is not sorted. Sorting is done in the
--   createBytestack function

-- | ==== __Examples__
-- >>> getRegisterFromMetabytes [([(0,13),(1,18)],Memory [[11,13],[15,18]]),([(0,14),(1,17)],Memory [[12,14],[16,17]]),([(0,3),(1,7)],Memory [[1,3],[5,7]]),([(0,4),(1,8)],Memory [[2,4],[6,8]])]
-- [(0,18),(1,17),(2,7),(3,8)]
getRegisterFromMetabytes :: [Bytestore] -> [Record]
getRegisterFromMetabytes metabytes = acc metabytes []
  where
    acc :: [Bytestore] -> [Record] -> [Record]
    acc [] refs = refs
    acc (metabyte : remainingMetabytes) refs = acc remainingMetabytes (refs ++ [(i, getTopBitFromBytestack metabyte)])
      where
        i = length refs

-- const getTopBitFromBytestack = (bytestack: Bytestack): number | null => {
--     const topRef = bytestack.register[bytestack.register.length - 1]
--     switch (typeof topRef) {
--         case "undefined": {
--             return null
--         }
--         default: return topRef.topBit
--     }
--
-- }

-- | Get the top Bit from a Bytestack

-- | The top Bit is the last Bit in the last Byte referenced in the last record
--   of the Bytestore referenced in the last record of the last Bytestore of...
--   and so on until you reach the top level of the Bytestack

-- | This is also expected to be the highest value in the Bytestack

-- | ==== __Examples__
-- >>> getTopBitFromBytestack (([(0,28),(1,38)],BigMemory [([(0,27),(1,28)],BigMemory [([(0,23),(1,27)],Memory [[21,23],[25,27]]),([(0,24),(1,28)],Memory [[22,24],[26,28]])]),([(1,37),(0,38)],BigMemory [([(0,33),(1,38)],Memory [[31,33],[35,38]]),([(0,34),(1,37)],Memory [[32,14],[36,37]])])]))
-- 38
getTopBitFromBytestack :: Bytestore -> Int
getTopBitFromBytestack (register, _) = snd (last register)
