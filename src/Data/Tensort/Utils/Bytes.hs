{-# LANGUAGE GADTs #-}

module Data.Tensort.Utils.Bytes
  ( convertRawBitsToBytes,
    getTensorFromBytes,
    getTensorstacksFromBytes,
    reduceTensorstacks,
    reduceTensorstacksSinglePass,
    createTensorstack,
    getRegisterFromTensors,
    getTopBitFromTensorstack,
    createTensor,
    getSortedBitsFromMetastack,
    mkTSProps,
    TensortProps,
  )
where

import Data.Maybe (isNothing)
import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types (Byte, Tensorstack, Tensor, Memory (..), Record, Sortable (SortInt, SortRec), fromSortInt, fromSortRec)

data TensortProps = TensortProps {bytesize :: Int, subAlgorithm :: Sortable -> Sortable}

mkTSProps :: Int -> (Sortable -> Sortable) -> TensortProps
mkTSProps bSize subAlg = TensortProps {bytesize = bSize, subAlgorithm = subAlg}

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
convertRawBitsToBytes :: [Int] -> TensortProps -> [Byte]
convertRawBitsToBytes bits tsProps = foldr acc [] (splitEvery (bytesize tsProps) bits)
  where
    acc :: [Int] -> [Byte] -> [Byte]
    acc byte bytes = bytes ++ [fromSortInt (subAlgorithm tsProps (SortInt byte))]

-- | Convert a list of Bytes to a list of Tensorstacks.

-- | This is accomplished by making a Tensor for each Byte, converting that
--   Tensor into a Tensorstack (these are equivalent terms - see type
--   definitions for more info) and collating the Tensorstacks into a list

-- | ==== __Examples__
--  >>> getTensorstacksFromBytes [[2,4],[6,8],[1,3],[5,7]] 2
--  [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
getTensorstacksFromBytes :: [Byte] -> TensortProps -> [Tensorstack]
getTensorstacksFromBytes bytes tsProps = foldr acc [] (splitEvery (bytesize tsProps) bytes)
  where
    acc :: [Byte] -> [Tensorstack] -> [Tensorstack]
    acc byte tensorstacks = tensorstacks ++ [getTensorFromBytes byte tsProps]

-- | Convert a list of Bytes to a Tensor

-- | We do this by loading the list of Bytes into the new Tensor's Memory
--   and adding a sorted Register containing References to each Byte in Memory

-- | Each Record contains an Address pointing to the index of the referenced
--   Byte and a TopBit containing the value of the last (i.e. highest) Bit in
--   the referenced Byte

-- | The Register is bubblesorted by the TopBits of each Record

-- | ==== __Examples__
--  >>> getTensorFromBytes [[2,4,6,8],[1,3,5,7]]
--  ([(1,7),(0,8)],ByteMem [[2,4,6,8],[1,3,5,7]])
getTensorFromBytes :: [Byte] -> TensortProps -> Tensor
getTensorFromBytes bytes tsProps = do
  let register = acc bytes [] 0
  let register' = fromSortRec (subAlgorithm tsProps (SortRec register))
  (register', ByteMem bytes)
  where
    acc :: [Byte] -> [Record] -> Int -> [Record]
    acc [] register _ = register
    acc ([] : remainingBytes) register i = acc remainingBytes register (i + 1)
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)

-- | Take a list of Tensorstacks and group them together in new
--   Tensorstacks, each containing bytesize number of Tensors (former
--   Tensorstacks), until the number of Tensorstacks is equal to the bytesize

-- | The Registers of the new Tensorstacks are bubblesorted, as usual

-- | ==== __Examples__
-- >>> reduceTensorstacks [([(0, 33), (1, 38)], ByteMem [[31, 33], [35, 38]]), ([(0, 34), (1, 37)], ByteMem [[32, 14], [36, 37]]), ([(0, 23), (1, 27)], ByteMem [[21, 23], [25, 27]]), ([(0, 24), (1, 28)], ByteMem [[22, 24], [26, 28]]),([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])] 2
-- ([(1,18),(0,38)],TensorMem [([(0,28),(1,38)],TensorMem [([(0,27),(1,28)],TensorMem [([(0,23),(1,27)],ByteMem [[21,23],[25,27]]),([(0,24),(1,28)],ByteMem [[22,24],[26,28]])]),([(1,37),(0,38)],TensorMem [([(0,33),(1,38)],ByteMem [[31,33],[35,38]]),([(0,34),(1,37)],ByteMem [[32,14],[36,37]])])]),([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])])])
reduceTensorstacks :: [Tensorstack] -> TensortProps -> Tensorstack
reduceTensorstacks tensorstacks tsProps = do
  let newTensorstacks = reduceTensorstacksSinglePass tensorstacks tsProps
  if length newTensorstacks <= bytesize tsProps
    then createTensorstack newTensorstacks tsProps
    else reduceTensorstacks newTensorstacks tsProps

-- | Take a list of Tensorstacks  and group them together in new
--   Tensorstacks each containing bytesize number of Tensors (former Tensorstacks)

-- | The Registers of the new Tensorstacks are bubblesorted, as usual

-- | ==== __Examples__
-- >>> reduceTensorstacksSinglePass [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])] 2
-- [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])]
reduceTensorstacksSinglePass :: [Tensorstack] -> TensortProps -> [Tensorstack]
reduceTensorstacksSinglePass tensorstacks tsProps = foldr acc [] (splitEvery (bytesize tsProps) tensorstacks)
  where
    acc :: [Tensorstack] -> [Tensorstack] -> [Tensorstack]
    acc tensorstack newTensorstacks = newTensorstacks ++ [createTensorstack tensorstack tsProps]

-- | Create a Tensorstack with the collated and bubblesorted References from the
--   Tensors as the Register and the original Tensors as the data

-- | ==== __Examples__
-- >>> createTensorstack [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])]
-- ([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])])
createTensorstack :: [Tensor] -> TensortProps -> Tensorstack
createTensorstack tensors tsProps = (fromSortRec (subAlgorithm tsProps (SortRec (getRegisterFromTensors tensors))), TensorMem tensors)

-- | Create a Tensor from a Memory
--   Aliases to getTensorFromBytes for ByteMem and createTensorstack for
--   TensorMem

-- | I expect to refactor to simplify this before initial release
createTensor :: Memory -> TensortProps -> Tensor
createTensor (ByteMem bytes) tsProps = getTensorFromBytes bytes tsProps
createTensor (TensorMem tensors) tsProps = createTensorstack tensors tsProps

-- | For each Tensor, produces a Record by combining the top bit of the
--  Tensor with an index value for its Address

-- | Note that this output is not sorted. Sorting is done in the
--   createTensorstack function

-- | ==== __Examples__
-- >>> getRegisterFromTensors [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
-- [(0,18),(1,17),(2,7),(3,8)]
getRegisterFromTensors :: [Tensor] -> [Record]
getRegisterFromTensors tensors = acc tensors []
  where
    acc :: [Tensor] -> [Record] -> [Record]
    acc [] records = records
    acc (([], _) : remainingTensors) records = acc remainingTensors records
    acc (tensor : remainingTensors) records = acc remainingTensors (records ++ [(i, getTopBitFromTensorstack tensor)])
      where
        i = length records

-- | Get the top Bit from a Tensorstack

-- | The top Bit is the last Bit in the last Byte referenced in the last record
--   of the Tensor referenced in the last record of the last Tensor of...
--   and so on until you reach the top level of the Tensorstack

-- | This is also expected to be the highest value in the Tensorstack

-- | ==== __Examples__
-- >>> getTopBitFromTensorstack (([(0,28),(1,38)],TensorMem [([(0,27),(1,28)],TensorMem [([(0,23),(1,27)],ByteMem [[21,23],[25,27]]),([(0,24),(1,28)],ByteMem [[22,24],[26,28]])]),([(1,37),(0,38)],TensorMem [([(0,33),(1,38)],ByteMem [[31,33],[35,38]]),([(0,34),(1,37)],ByteMem [[32,14],[36,37]])])]))
-- 38
getTopBitFromTensorstack :: Tensor -> Int
getTopBitFromTensorstack (register, _) = snd (last register)

-- | Compile a sorted list of Bits from a list of Tensorstacks

-- | ==== __Examples__
--  >>> getSortedBitsFromMetastack ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--  [1,3,5,7]
--  >>> getSortedBitsFromMetastack ([(0,8),(1,18)],TensorMem [([(0,7),(1,8)],TensorMem [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]),([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]])])])
--  [1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18]
getSortedBitsFromMetastack :: Tensorstack -> TensortProps -> [Int]
getSortedBitsFromMetastack metastackRaw tsProps = acc metastackRaw []
  where
    acc :: Tensorstack -> [Int] -> [Int]
    acc metastack sortedBits = do
      let (nextBit, metastack') = removeTopBitFromTensor metastack tsProps
      if isNothing metastack'
        then nextBit : sortedBits
        else do
          acc (fromJust metastack') (nextBit : sortedBits)

-- | For use in compiling a list of Tensors into a sorted list of Bits
--
-- | Removes the top Bit from a Tensor, rebalances the Tensor and returns
--   the removed Bit along with the rebalanced Tensor

-- | ==== __Examples__
--   >>> removeTopBitFromTensor  ([(0,5),(1,7)],ByteMem [[1,5],[3,7]])
--   (7,Just ([(1,3),(0,5)],ByteMem [[1,5],[3]]))
removeTopBitFromTensor :: Tensor -> TensortProps -> (Int, Maybe Tensor)
removeTopBitFromTensor (register, memory) tsProps = do
  let topRecord = last register
  let topAddress = fst topRecord
  let (topBit, memory') = removeBitFromMemory memory topAddress tsProps
  if isNothing memory'
    then (topBit, Nothing)
    else (topBit, Just (createTensor (fromJust memory') tsProps))

-- | ==== __Examples__
removeBitFromMemory :: Memory -> Int -> TensortProps -> (Int, Maybe Memory)
removeBitFromMemory (ByteMem bytes) i tsProps = do
  let topByte = bytes !! i
  let topBit = last topByte
  let topByte' = init topByte
  case length topByte' of
    0 -> do
      let bytes' = take i bytes ++ drop (i + 1) bytes
      if null bytes'
        then (topBit, Nothing)
        else (topBit, Just (ByteMem bytes'))
    1 -> do
      let bytes' = take i bytes ++ [topByte'] ++ drop (i + 1) bytes
      (topBit, Just (ByteMem bytes'))
    _ -> do
      let topByte'' = fromSortInt (subAlgorithm tsProps (SortInt topByte'))
      let bytes' = take i bytes ++ [topByte''] ++ drop (i + 1) bytes
      (topBit, Just (ByteMem bytes'))
removeBitFromMemory (TensorMem tensors) i tsProps = do
  let topTensor = tensors !! i
  let (topBit, topTensor') = removeTopBitFromTensor topTensor tsProps
  if isNothing topTensor'
    then do
      let tensors' = take i tensors ++ drop (i + 1) tensors
      if null tensors'
        then (topBit, Nothing)
        else (topBit, Just (TensorMem tensors'))
    else do
      let tensors' = take i tensors ++ [fromJust topTensor'] ++ drop (i + 1) tensors
      (topBit, Just (TensorMem tensors'))

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
