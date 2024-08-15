module Data.Tensort.Utils.Compose
  ( createInitialTensors,
    createTensor,
  )
where

import Data.Tensort.Utils.SimplifyRegister
  ( applySortingFromSimplifiedRegister,
    simplifyRegister,
  )
import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types
  ( Byte,
    ByteR,
    Memory (..),
    MemoryR (..),
    Record,
    RecordR,
    SBit (..),
    SBytes (..),
    SMemory (..),
    SRecord (..),
    STensor (..),
    STensors (..),
    SortAlg,
    Sortable (..),
    Tensor,
    TensorR,
    TensortProps (..),
    WonkyState,
  )

-- | Convert a list of Bytes to a list of TensorStacks.

-- | This is accomplished by making a Tensor for each Byte, converting that
--   Tensor into a TensorStack (these are equivalent terms - see type
--   definitions for more info) and collating the TensorStacks into a list

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> createInitialTensors (mkTsProps 2 bubblesort) (SBytesBit [[2,4],[6,8],[1,3],[5,7]])
-- STensorsBit [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
createInitialTensors :: TensortProps -> WonkyState -> SBytes -> (STensors, WonkyState)
createInitialTensors tsProps wonkySt (SBytesBit bytes) = do
  let (result, wonkySt') = createInitialTensorsBits tsProps wonkySt bytes
  (STensorsBit result, wonkySt')
createInitialTensors tsProps wonkySt (SBytesRec recs) = do
  let (result, wonkySt') = createInitialTensorsRecs tsProps wonkySt recs
  (STensorsRec result, wonkySt')

createInitialTensorsBits :: TensortProps -> WonkyState -> [Byte] -> ([Tensor], WonkyState)
createInitialTensorsBits tsProps wonkySt bytes = foldr acc ([], wonkySt) (splitEvery (bytesize tsProps) bytes)
  where
    acc :: [Byte] -> ([Tensor], WonkyState) -> ([Tensor], WonkyState)
    acc byte (tensorStacks, wonkySt') = do
      let (tensor, wonkySt'') = getTensorFromBytes (subAlgorithm tsProps) wonkySt' (SBytesBit byte)
      (tensorStacks ++ [fromSTensorBit tensor], wonkySt'')

createInitialTensorsRecs :: TensortProps -> WonkyState -> [ByteR] -> ([TensorR], WonkyState)
createInitialTensorsRecs tsProps wonkySt bytesR = foldr acc ([], wonkySt) (splitEvery (bytesize tsProps) bytesR)
  where
    acc :: [ByteR] -> ([TensorR], WonkyState) -> ([TensorR], WonkyState)
    acc byteR (tensorStacks, wonkySt') = do
      let (tensor, wonkySt'') = getTensorFromBytes (subAlgorithm tsProps) wonkySt' (SBytesRec byteR)
      (tensorStacks ++ [fromSTensorRec tensor], wonkySt'')

-- | Create a Tensor from a Memory
--   Aliases to getTensorFromBytes for ByteMem and getTensorFromTensors for
--   TensorMem
createTensor :: SortAlg -> WonkyState -> SMemory -> (STensor, WonkyState)
createTensor subAlg wonkySt (SMemoryBit memory) = createTensorB subAlg wonkySt memory
createTensor subAlg wonkySt (SMemoryRec memoryR) = createTensorR subAlg wonkySt memoryR

createTensorB :: SortAlg -> WonkyState -> Memory -> (STensor, WonkyState)
createTensorB subAlg wonkySt (ByteMem bytes) = getTensorFromBytes subAlg wonkySt (SBytesBit bytes)
createTensorB subAlg wonkySt (TensorMem tensors) = getTensorFromTensors subAlg wonkySt (STensorsBit tensors)

createTensorR :: SortAlg -> WonkyState -> MemoryR -> (STensor, WonkyState)
createTensorR subAlg wonkySt (ByteMemR bytesR) = getTensorFromBytes subAlg wonkySt (SBytesRec bytesR)
createTensorR subAlg wonkySt (TensorMemR tensorsR) = getTensorFromTensors subAlg wonkySt (STensorsRec tensorsR)

-- | Convert a list of Bytes to a Tensor

-- | We do this by loading the list of Bytes into the new Tensor's Memory
--   and adding a sorted Register containing References to each Byte in Memory

-- | Each Record contains an Address pointing to the index of the referenced
--   Byte and a TopBit containing the value of the last (i.e. highest) Bit in
--   the referenced Byte

-- | The Register is sorted by the TopBits of each Record

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getTensorFromBytes bubblesort (SBytesBit [[2,4,6,8],[1,3,5,7]])
-- STensorBit ([(1,7),(0,8)],ByteMem [[2,4,6,8],[1,3,5,7]])
getTensorFromBytes :: SortAlg -> WonkyState -> SBytes -> (STensor, WonkyState)
getTensorFromBytes subAlg wonkySt (SBytesBit bytes) = do
  let (result, wonkySt') = getTensorFromBytesB subAlg wonkySt bytes
  (STensorBit result, wonkySt')
getTensorFromBytes subAlg wonkySt (SBytesRec recs) = do
  let (result, wonkySt') = getTensorFromBytesR subAlg wonkySt recs
  (STensorRec result, wonkySt')

getTensorFromBytesB :: SortAlg -> WonkyState -> [Byte] -> (Tensor, WonkyState)
getTensorFromBytesB subAlg wonkySt bytes = do
  let register = acc bytes [] 0
  let (result, wonkySt') = subAlg wonkySt (SortRec register)
  let register' = fromSortRec result
  ((register', ByteMem bytes), wonkySt')
  where
    acc :: [Byte] -> [Record] -> Int -> [Record]
    acc [] register _ = register
    acc ([] : remainingBytes) register i = acc remainingBytes register (i + 1)
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)

getTensorFromBytesR :: SortAlg -> WonkyState -> [ByteR] -> (TensorR, WonkyState)
getTensorFromBytesR subAlg wonkySt bytesR = do
  let registerR = acc bytesR [] 0
  let simplifiedRegiser = simplifyRegister registerR
  let (simplifiedRegiser', wonkySt') = subAlg wonkySt (SortRec simplifiedRegiser)
  let registerR' = applySortingFromSimplifiedRegister (fromSortRec simplifiedRegiser') registerR
  ((registerR', ByteMemR bytesR), wonkySt')
  where
    acc :: [ByteR] -> [RecordR] -> Int -> [RecordR]
    acc [] register _ = register
    acc ([] : remainingBytesR) registerR i = acc remainingBytesR registerR (i + 1)
    acc (byteR : remainingBytesR) registerR i = acc remainingBytesR (registerR ++ [(i, last byteR)]) (i + 1)

-- | Create a TensorStack with the collated and sorted References from the
--   Tensors as the Register and the original Tensors as the data

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getTensorFromTensors bubblesort (STensorsBit [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])])
-- STensorBit ([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])])
getTensorFromTensors :: SortAlg -> WonkyState -> STensors -> (STensor, WonkyState)
getTensorFromTensors subAlg wonkySt (STensorsBit tensors) = do
  let (result, wonkySt') = getTensorFromTensorsB subAlg wonkySt tensors
  (STensorBit result, wonkySt')
getTensorFromTensors subAlg wonkySt (STensorsRec tensors) = do
  let (result, wonkySt') = getTensorFromTensorsR subAlg wonkySt tensors
  (STensorRec result, wonkySt')

getTensorFromTensorsB :: SortAlg -> WonkyState -> [Tensor] -> (Tensor, WonkyState)
getTensorFromTensorsB subAlg wonkySt tensors = do
  let register = getRegisterFromTensors (STensorsBit tensors)
  let (result, wonkySt') = subAlg wonkySt (SortRec (fromSRecordArrayBit register))
  ((fromSortRec result, TensorMem tensors), wonkySt')

getTensorFromTensorsR :: SortAlg -> WonkyState -> [TensorR] -> (TensorR, WonkyState)
getTensorFromTensorsR subAlg wonkySt tensorsR = do
  let registerR = getRegisterFromTensors (STensorsRec tensorsR)
  let simplifiedRegiser = simplifyRegister (fromSRecordArrayRec registerR)
  let (simplifiedRegiser', wonkySt') = subAlg wonkySt (SortRec simplifiedRegiser)
  let registerR' = applySortingFromSimplifiedRegister (fromSortRec simplifiedRegiser') (fromSRecordArrayRec registerR)
  ((registerR', TensorMemR tensorsR), wonkySt')

-- | For each Tensor, produces a Record by combining the top bit of the
--  Tensor with an index value for its Address

-- | Note that this output is not sorted. Sorting is done in the
--   getTensorFromTensors function

-- | ==== __Examples__
-- >>> getRegisterFromTensors (STensorsBit [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])])
-- [SRecordBit (0,18),SRecordBit (1,17),SRecordBit (2,7),SRecordBit (3,8)]
getRegisterFromTensors :: STensors -> [SRecord]
getRegisterFromTensors (STensorsBit tensors) = getRegisterFromTensorsB tensors
getRegisterFromTensors (STensorsRec tensors) = getRegisterFromTensorsR tensors

getRegisterFromTensorsB :: [Tensor] -> [SRecord]
getRegisterFromTensorsB tensors = acc tensors []
  where
    acc :: [Tensor] -> [SRecord] -> [SRecord]
    acc [] records = records
    acc (([], _) : remainingTensors) records = acc remainingTensors records
    acc (tensor : remainingTensors) records = acc remainingTensors (records ++ [SRecordBit (i, fromSBitBit (getTopBitFromTensorStack (STensorBit tensor)))])
      where
        i = length records

getRegisterFromTensorsR :: [TensorR] -> [SRecord]
getRegisterFromTensorsR tensorsR = acc tensorsR []
  where
    acc :: [TensorR] -> [SRecord] -> [SRecord]
    acc [] records = records
    acc (([], _) : remainingTensorsR) records = acc remainingTensorsR records
    acc (tensorR : remainingTensorsR) records = acc remainingTensorsR (records ++ [SRecordRec (i, fromSBitRec (getTopBitFromTensorStack (STensorRec tensorR)))])
      where
        i = length records

-- | Get the top Bit from a TensorStack

-- | The top Bit is the last Bit in the last Byte referenced in the last record
--   of the Tensor referenced in the last record of the last Tensor of...
--   and so on until you reach the top level of the TensorStack

-- | This is also expected to be the highest value in the TensorStack

-- | ==== __Examples__
-- >>> getTopBitFromTensorStack (STensorBit ([(0,28),(1,38)],TensorMem [([(0,27),(1,28)],TensorMem [([(0,23),(1,27)],ByteMem [[21,23],[25,27]]),([(0,24),(1,28)],ByteMem [[22,24],[26,28]])]),([(1,37),(0,38)],TensorMem [([(0,33),(1,38)],ByteMem [[31,33],[35,38]]),([(0,34),(1,37)],ByteMem [[32,14],[36,37]])])]))
-- SBitBit 38
getTopBitFromTensorStack :: STensor -> SBit
getTopBitFromTensorStack (STensorBit tensor) = getTopBitFromTensorStackB tensor
getTopBitFromTensorStack (STensorRec tensorR) = getTopBitFromTensorStackR tensorR

getTopBitFromTensorStackB :: Tensor -> SBit
getTopBitFromTensorStackB (register, _) = SBitBit (snd (last register))

getTopBitFromTensorStackR :: TensorR -> SBit
getTopBitFromTensorStackR (registerR, _) = SBitRec (snd (last registerR))
