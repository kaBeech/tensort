{-# LANGUAGE GADTs #-}

module Data.Robustsort.Utils.Bytes
  ( convertRawBitsToBytes,
    getBytestoreFromBytes,
    getBytestacksFromBytes,
    reduceBytestacks,
    reduceBytestacksSinglePass,
    createBytestack,
    getRegisterFromMetabytes,
    getTopBitFromBytestack,
    createBytestore,
    getSortedBitsFromMetastack,
    mkBSProps,
    BytesortProps,
  )
where

import Data.Maybe (isNothing)
import Data.Robustsort.Subalgorithms.Bubblesort (bubblesort)
import Data.Robustsort.Utils.Split (splitEvery)
import Data.Robustsort.Utils.Types (Byte, Bytestack, Bytestore, Memory (..), Record, Sortable (SortInt, SortRec), fromSortInt, fromSortRec)

data BytesortProps = BytesortProps {bytesize :: Int, subAlgorithm :: Sortable -> Sortable}

mkBSProps :: Int -> (Sortable -> Sortable) -> BytesortProps
mkBSProps bSize subAlg = BytesortProps {bytesize = bSize, subAlgorithm = subAlg}

-- | Convert a list of Bits to a list of Bytes of given bytesize, bubblesorting
--   each byte.

-- | ==== __Examples__
--   >>> convertRawBitsToBytes [5,1,3,7,8,2,4,6] 4
--   [[2,4,6,8],[1,3,5,7]]
-- convertRawBitsToBytes :: [Int] -> Int -> [Byte]
-- convertRawBitsToBytes bits bytesize = foldr acc [] (splitEvery bytesize bits)
--   where
--     acc :: [Int] -> [Byte] -> [Byte]
--     acc byte bytes = bytes ++ [fromSortInt (bubblesort (SortInt byte))]
convertRawBitsToBytes :: [Int] -> BytesortProps -> [Byte]
convertRawBitsToBytes bits bsProps = foldr acc [] (splitEvery (bytesize bsProps) bits)
  where
    acc :: [Int] -> [Byte] -> [Byte]
    acc byte bytes = bytes ++ [fromSortInt (bubblesort (SortInt byte))]

-- | Convert a list of Bytes to a list of Bytestacks.

-- | This is accomplished by making a Bytestore for each Byte, converting that
--   Bytestore into a Bytestack (these are equivalent terms - see type
--   definitions for more info) and collating the Bytestacks into a list

-- | ==== __Examples__
--  >>> getBytestacksFromBytes [[2,4],[6,8],[1,3],[5,7]] 2
--  [([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])]
getBytestacksFromBytes :: [Byte] -> BytesortProps -> [Bytestack]
getBytestacksFromBytes bytes bsProps = foldr acc [] (splitEvery (bytesize bsProps) bytes)
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
--  ([(1,7),(0,8)],SmallMemory [[2,4,6,8],[1,3,5,7]])
getBytestoreFromBytes :: [Byte] -> Bytestore
getBytestoreFromBytes bytes = do
  let register = acc bytes [] 0
  let register' = fromSortRec (bubblesort (SortRec register))
  (register', SmallMemory bytes)
  where
    acc :: [Byte] -> [Record] -> Int -> [Record]
    acc [] register _ = register
    acc ([] : remainingBytes) register i = acc remainingBytes register (i + 1)
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)

-- | Take a list of Bytestacks (Metabytes) and group them together in new
--   Bytestacks, each containing bytesize number of Metabytes (former
--   Bytestacks), until the number of Bytestacks is equal to the bytesize

-- | The Registers of the new Bytestacks are bubblesorted, as usual

-- | ==== __Examples__
-- >>> reduceBytestacks [([(0, 33), (1, 38)], SmallMemory [[31, 33], [35, 38]]), ([(0, 34), (1, 37)], SmallMemory [[32, 14], [36, 37]]), ([(0, 23), (1, 27)], SmallMemory [[21, 23], [25, 27]]), ([(0, 24), (1, 28)], SmallMemory [[22, 24], [26, 28]]),([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(0,14),(1,17)],SmallMemory [[12,14],[16,17]]),([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])] 2
-- ([(1,18),(0,38)],BigMemory [([(0,28),(1,38)],BigMemory [([(0,27),(1,28)],BigMemory [([(0,23),(1,27)],SmallMemory [[21,23],[25,27]]),([(0,24),(1,28)],SmallMemory [[22,24],[26,28]])]),([(1,37),(0,38)],BigMemory [([(0,33),(1,38)],SmallMemory [[31,33],[35,38]]),([(0,34),(1,37)],SmallMemory [[32,14],[36,37]])])]),([(0,8),(1,18)],BigMemory [([(0,7),(1,8)],BigMemory [([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])]),([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(0,14),(1,17)],SmallMemory [[12,14],[16,17]])])])])
reduceBytestacks :: [Bytestack] -> BytesortProps -> Bytestack
reduceBytestacks bytestacks bsProps = do
  let newBytestacks = reduceBytestacksSinglePass bytestacks bsProps
  if length newBytestacks <= bytesize bsProps
    then createBytestack newBytestacks
    else reduceBytestacks newBytestacks bsProps

-- | Take a list of Bytestacks (Metabytes) and group them together in new
--   Bytestacks each containing bytesize number of Metabytes (former Bytestacks)

-- | The Registers of the new Bytestacks are bubblesorted, as usual

-- | ==== __Examples__
-- >>> reduceBytestacksSinglePass [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(0,14),(1,17)],SmallMemory [[12,14],[16,17]]),([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])] 2
-- [([(0,7),(1,8)],BigMemory [([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])]),([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(0,14),(1,17)],SmallMemory [[12,14],[16,17]])])]
reduceBytestacksSinglePass :: [Bytestack] -> BytesortProps -> [Bytestack]
reduceBytestacksSinglePass bytestacks bsProps = foldr acc [] (splitEvery (bytesize bsProps) bytestacks)
  where
    acc :: [Bytestack] -> [Bytestack] -> [Bytestack]
    acc bytestack newBytestacks = newBytestacks ++ [createBytestack bytestack]

-- | Create a Bytestack with the collated and bubblesorted References from the
--   Metabytes as the Register and the original Metabytes as the data

-- | ==== __Examples__
-- >>> createBytestack [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(1,14),(0,17)],SmallMemory [[16,17],[12,14]])]
-- ([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(1,14),(0,17)],SmallMemory [[16,17],[12,14]])])
createBytestack :: [Bytestore] -> Bytestack
createBytestack metabytes = (fromSortRec (bubblesort (SortRec (getRegisterFromMetabytes metabytes))), BigMemory metabytes)

-- | Create a Bytestore from a Memory
--   Aliases to getBytestoreFromBytes for SmallMemory and createBytestack for
--   BigMemory

-- | I expect to refactor to simplify this before initial release
createBytestore :: Memory -> Bytestore
createBytestore (SmallMemory bytes) = getBytestoreFromBytes bytes
createBytestore (BigMemory metabytes) = createBytestack metabytes

-- | For each Bytestore, produces a Record by combining the top bit of the
--  Bytestore with an index value for its Address

-- | Note that this output is not sorted. Sorting is done in the
--   createBytestack function

-- | ==== __Examples__
-- >>> getRegisterFromMetabytes [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(0,14),(1,17)],SmallMemory [[12,14],[16,17]]),([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])]
-- [(0,18),(1,17),(2,7),(3,8)]
getRegisterFromMetabytes :: [Bytestore] -> [Record]
getRegisterFromMetabytes metabytes = acc metabytes []
  where
    acc :: [Bytestore] -> [Record] -> [Record]
    acc [] records = records
    acc (([], _) : remainingMetabytes) records = acc remainingMetabytes records
    acc (metabyte : remainingMetabytes) records = acc remainingMetabytes (records ++ [(i, getTopBitFromBytestack metabyte)])
      where
        i = length records

-- | Get the top Bit from a Bytestack

-- | The top Bit is the last Bit in the last Byte referenced in the last record
--   of the Bytestore referenced in the last record of the last Bytestore of...
--   and so on until you reach the top level of the Bytestack

-- | This is also expected to be the highest value in the Bytestack

-- | ==== __Examples__
-- >>> getTopBitFromBytestack (([(0,28),(1,38)],BigMemory [([(0,27),(1,28)],BigMemory [([(0,23),(1,27)],SmallMemory [[21,23],[25,27]]),([(0,24),(1,28)],SmallMemory [[22,24],[26,28]])]),([(1,37),(0,38)],BigMemory [([(0,33),(1,38)],SmallMemory [[31,33],[35,38]]),([(0,34),(1,37)],SmallMemory [[32,14],[36,37]])])]))
-- 38
getTopBitFromBytestack :: Bytestore -> Int
getTopBitFromBytestack (register, _) = snd (last register)

-- | Compile a sorted list of Bits from a list of Bytestacks

-- | ==== __Examples__
--  >>> getSortedBitsFromMetastack ([(0,5),(1,7)],SmallMemory [[1,5],[3,7]])
--  [1,3,5,7]
--  >>> getSortedBitsFromMetastack ([(0,8),(1,18)],BigMemory [([(0,7),(1,8)],BigMemory [([(0,3),(1,7)],SmallMemory [[1,3],[5,7]]),([(0,4),(1,8)],SmallMemory [[2,4],[6,8]])]),([(1,17),(0,18)],BigMemory [([(0,13),(1,18)],SmallMemory [[11,13],[15,18]]),([(0,14),(1,17)],SmallMemory [[12,14],[16,17]])])])
--  [1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18]
getSortedBitsFromMetastack :: Bytestack -> [Int]
getSortedBitsFromMetastack metastackRaw = acc metastackRaw []
  where
    acc :: Bytestack -> [Int] -> [Int]
    acc metastack sortedBits = do
      let (nextBit, metastack') = removeTopBitFromBytestore metastack
      if isNothing metastack'
        then nextBit : sortedBits
        else do
          acc (fromJust metastack') (nextBit : sortedBits)

-- | For use in compiling a list of Bytestores into a sorted list of Bits
--
-- | Removes the top Bit from a Bytestore, rebalances the Bytestore and returns
--   the removed Bit along with the rebalanced Bytestore

-- | ==== __Examples__
--   >>> removeTopBitFromBytestore  ([(0,5),(1,7)],SmallMemory [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],SmallMemory [[1,5],[3]]))
removeTopBitFromBytestore :: Bytestore -> (Int, Maybe Bytestore)
removeTopBitFromBytestore (register, memory) = do
  let topRecord = last register
  let topAddress = fst topRecord
  let (topBit, memory') = removeBitFromMemory memory topAddress
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, Just (createBytestore (fromJust memory')))

-- | ==== __Examples__
removeBitFromMemory :: Memory -> Int -> (Int, Maybe Memory)
removeBitFromMemory (SmallMemory bytes) i = do
  let topByte = bytes !! i
  let topBit = last topByte
  let topByte' = init topByte
  if null topByte'
    then do
      let bytes' = take i bytes ++ drop (i + 1) bytes
      if null bytes'
        then (topBit, Nothing)
        else (topBit, Just (SmallMemory bytes'))
    else do
      let bytes' = take i bytes ++ [topByte'] ++ drop (i + 1) bytes
      (topBit, Just (SmallMemory bytes'))
removeBitFromMemory (BigMemory bytestores) i = do
  let topBytestore = bytestores !! i
  let (topBit, topBytestore') = removeTopBitFromBytestore topBytestore
  if isNothing topBytestore'
    then do
      let bytestores' = take i bytestores ++ drop (i + 1) bytestores
      if null bytestores'
        then (topBit, Nothing)
        else (topBit, Just (BigMemory bytestores'))
    else do
      let bytestores' = take i bytestores ++ [fromJust topBytestore'] ++ drop (i + 1) bytestores
      (topBit, Just (BigMemory bytestores'))

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
