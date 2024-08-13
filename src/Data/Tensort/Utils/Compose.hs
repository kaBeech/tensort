module Data.Tensort.Utils.Compose
  ( createInitialTensors,
    createTensor,
  )
where

import qualified Data.Bifunctor
import Data.Data (ConstrRep (..))
import Data.Tensort.Utils.SimplifyRegister (applySortingFromSimplifiedRegister, simplifyRegister)
import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types (Bit, Byte, ByteR, Memory (..), MemoryR (..), Record, RecordR, SBit (..), SBytes (..), SMemory (..), SRecord (..), SRecords (SRecordsBit), STensor (..), STensors (..), SortAlg, Sortable (..), Tensor, TensorR, TensortProps (..), fromSBitBit, fromSBitRec, fromSRecordArrayBit, fromSRecordArrayRec, fromSRecordBit, fromSRecordsBit, fromSTensorBit, fromSTensorRec, fromSTensorsBit, fromSortRec)
import GHC.Base (RuntimeRep (TupleRep))

-- | Convert a list of Bytes to a list of TensorStacks.

-- | This is accomplished by making a Tensor for each Byte, converting that
--   Tensor into a TensorStack (these are equivalent terms - see type
--   definitions for more info) and collating the TensorStacks into a list

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> import Data.Tensort.Utils.MkTsProps (mkTsProps)
-- >>> createInitialTensors (mkTsProps 2 bubblesort) [[2,4],[6,8],[1,3],[5,7]]
-- [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
createInitialTensors :: TensortProps -> SBytes -> STensors
createInitialTensors tsProps (SBytesBit bytes) = STensorsBit (createInitialTensorsBits tsProps bytes)
createInitialTensors tsProps (SBytesRec recs) = STensorsRec (createInitialTensorsRecs tsProps recs)

createInitialTensorsBits :: TensortProps -> [Byte] -> [Tensor]
createInitialTensorsBits tsProps bytes = foldr acc [] (splitEvery (bytesize tsProps) bytes)
  where
    acc :: [Byte] -> [Tensor] -> [Tensor]
    acc byte tensorStacks = tensorStacks ++ [fromSTensorBit (getTensorFromBytes (subAlgorithm tsProps) (SBytesBit byte))]

createInitialTensorsRecs :: TensortProps -> [ByteR] -> [TensorR]
createInitialTensorsRecs tsProps bytesR = foldr acc [] (splitEvery (bytesize tsProps) bytesR)
  where
    acc :: [ByteR] -> [TensorR] -> [TensorR]
    acc byteR tensorStacks = tensorStacks ++ [fromSTensorRec (getTensorFromBytes (subAlgorithm tsProps) (SBytesRec byteR))]

-- | Create a Tensor from a Memory
--   Aliases to getTensorFromBytes for ByteMem and getTensorFromTensors for
--   TensorMem
createTensor :: SortAlg -> SMemory -> STensor
createTensor subAlg (SMemoryBit memory) = createTensorB subAlg memory
createTensor subAlg (SMemoryRec memoryR) = createTensorR subAlg memoryR

createTensorB :: SortAlg -> Memory -> STensor
createTensorB subAlg (ByteMem bytes) = getTensorFromBytes subAlg (SBytesBit bytes)
createTensorB subAlg (TensorMem tensors) = getTensorFromTensors subAlg (STensorsBit tensors)

createTensorR :: SortAlg -> MemoryR -> STensor
createTensorR subAlg (ByteMemR bytesR) = getTensorFromBytes subAlg (SBytesRec bytesR)
createTensorR subAlg (TensorMemR tensorsR) = getTensorFromTensors subAlg (STensorsRec tensorsR)

-- | Convert a list of Bytes to a Tensor

-- | We do this by loading the list of Bytes into the new Tensor's Memory
--   and adding a sorted Register containing References to each Byte in Memory

-- | Each Record contains an Address pointing to the index of the referenced
--   Byte and a TopBit containing the value of the last (i.e. highest) Bit in
--   the referenced Byte

-- | The Register is sorted by the TopBits of each Record

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getTensorFromBytes bubblesort [[2,4,6,8],[1,3,5,7]]
-- ([(1,7),(0,8)],ByteMem [[2,4,6,8],[1,3,5,7]])
getTensorFromBytes :: SortAlg -> SBytes -> STensor
getTensorFromBytes subAlg (SBytesBit bytes) = STensorBit (getTensorFromBytesB subAlg bytes)
getTensorFromBytes subAlg (SBytesRec recs) = STensorRec (getTensorFromBytesR subAlg recs)

getTensorFromBytesB :: SortAlg -> [Byte] -> Tensor
getTensorFromBytesB subAlg bytes = do
  let register = acc bytes [] 0
  let register' = fromSortRec (subAlg (SortRec register))
  (register', ByteMem bytes)
  where
    acc :: [Byte] -> [Record] -> Int -> [Record]
    acc [] register _ = register
    acc ([] : remainingBytes) register i = acc remainingBytes register (i + 1)
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)

getTensorFromBytesR :: SortAlg -> [ByteR] -> TensorR
getTensorFromBytesR subAlg bytesR = do
  let registerR = acc bytesR [] 0
  let simplifiedRegiser = simplifyRegister registerR
  let simplifiedRegiser' = fromSortRec (subAlg (SortRec simplifiedRegiser))
  let registerR' = applySortingFromSimplifiedRegister simplifiedRegiser' registerR
  (registerR', ByteMemR bytesR)
  where
    acc :: [ByteR] -> [RecordR] -> Int -> [RecordR]
    acc [] register _ = register
    acc ([] : remainingBytesR) registerR i = acc remainingBytesR registerR (i + 1)
    acc (byteR : remainingBytesR) registerR i = acc remainingBytesR (registerR ++ [(i, last byteR)]) (i + 1)

-- | Create a TensorStack with the collated and sorted References from the
--   Tensors as the Register and the original Tensors as the data

-- | ==== __Examples__
-- >>> import Data.Tensort.Subalgorithms.Bubblesort (bubblesort)
-- >>> getTensorFromTensors bubblesort [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])]
-- ([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])])
getTensorFromTensors :: SortAlg -> STensors -> STensor
getTensorFromTensors subAlg (STensorsBit tensors) = STensorBit (getTensorFromTensorsB subAlg tensors)
getTensorFromTensors subAlg (STensorsRec tensors) = STensorRec (getTensorFromTensorsR subAlg tensors)

getTensorFromTensorsB :: SortAlg -> [Tensor] -> Tensor
getTensorFromTensorsB subAlg tensors = (fromSortRec (subAlg (SortRec (fromSRecordArrayBit (getRegisterFromTensors (STensorsBit tensors))))), TensorMem tensors)

getTensorFromTensorsR :: SortAlg -> [TensorR] -> TensorR
getTensorFromTensorsR subAlg tensorsR = do
  let registerR = getRegisterFromTensors (STensorsRec tensorsR)
  let simplifiedRegiser = simplifyRegister (fromSRecordArrayRec registerR)
  let simplifiedRegiser' = fromSortRec (subAlg (SortRec simplifiedRegiser))
  let registerR' = applySortingFromSimplifiedRegister simplifiedRegiser' (fromSRecordArrayRec registerR)
  (registerR', TensorMemR tensorsR)

-- | For each Tensor, produces a Record by combining the top bit of the
--  Tensor with an index value for its Address

-- | Note that this output is not sorted. Sorting is done in the
--   getTensorFromTensors function

-- | ==== __Examples__
-- >>> getRegisterFromTensors [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
-- [(0,18),(1,17),(2,7),(3,8)]
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
-- >>> getTopBitFromTensorStack (([(0,28),(1,38)],TensorMem [([(0,27),(1,28)],TensorMem [([(0,23),(1,27)],ByteMem [[21,23],[25,27]]),([(0,24),(1,28)],ByteMem [[22,24],[26,28]])]),([(1,37),(0,38)],TensorMem [([(0,33),(1,38)],ByteMem [[31,33],[35,38]]),([(0,34),(1,37)],ByteMem [[32,14],[36,37]])])]))
-- 38
getTopBitFromTensorStack :: STensor -> SBit
getTopBitFromTensorStack (STensorBit tensor) = getTopBitFromTensorStackB tensor
getTopBitFromTensorStack (STensorRec tensorR) = getTopBitFromTensorStackR tensorR

getTopBitFromTensorStackB :: Tensor -> SBit
getTopBitFromTensorStackB (register, _) = SBitBit (snd (last register))

getTopBitFromTensorStackR :: TensorR -> SBit
getTopBitFromTensorStackR (registerR, _) = SBitRec (snd (last registerR))
