module Data.Tensort.Utils.Compose
  ( createInitialTensors,
    createTensor,
  )
where

import Data.Tensort.Utils.Split (splitEvery)
import Data.Tensort.Utils.Types (Byte, Memory (..), Record, SortAlg, Sortable (..), Tensor, TensortProps (..), fromSortRec)

-- | Convert a list of Bytes to a list of TensorStacks.

-- | This is accomplished by making a Tensor for each Byte, converting that
--   Tensor into a TensorStack (these are equivalent terms - see type
--   definitions for more info) and collating the TensorStacks into a list

-- | ==== __Examples__
--  >>> createInitialTensors [[2,4],[6,8],[1,3],[5,7]] 2
--  [([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
createInitialTensors :: [Byte] -> TensortProps -> [Tensor]
createInitialTensors bytes tsProps = foldr acc [] (splitEvery (bytesize tsProps) bytes)
  where
    acc :: [Byte] -> [Tensor] -> [Tensor]
    acc byte tensorStacks = tensorStacks ++ [getTensorFromBytes byte (subAlgorithm tsProps)]

-- | Create a Tensor from a Memory
--   Aliases to getTensorFromBytes for ByteMem and getTensorFromTensors for
--   TensorMem
createTensor :: Memory -> SortAlg -> Tensor
createTensor (ByteMem bytes) subAlg = getTensorFromBytes bytes subAlg
createTensor (TensorMem tensors) subAlg = getTensorFromTensors tensors subAlg

-- | Convert a list of Bytes to a Tensor

-- | We do this by loading the list of Bytes into the new Tensor's Memory
--   and adding a sorted Register containing References to each Byte in Memory

-- | Each Record contains an Address pointing to the index of the referenced
--   Byte and a TopBit containing the value of the last (i.e. highest) Bit in
--   the referenced Byte

-- | The Register is sorted by the TopBits of each Record

-- | ==== __Examples__
--  >>> getTensorFromBytes [[2,4,6,8],[1,3,5,7]]
--  ([(1,7),(0,8)],ByteMem [[2,4,6,8],[1,3,5,7]])
getTensorFromBytes :: [Byte] -> SortAlg -> Tensor
getTensorFromBytes bytes subAlg = do
  let register = acc bytes [] 0
  let register' = fromSortRec (subAlg (SortRec register))
  (register', ByteMem bytes)
  where
    acc :: [Byte] -> [Record] -> Int -> [Record]
    acc [] register _ = register
    acc ([] : remainingBytes) register i = acc remainingBytes register (i + 1)
    acc (byte : remainingBytes) register i = acc remainingBytes (register ++ [(i, last byte)]) (i + 1)

-- | Create a TensorStack with the collated and sorted References from the
--   Tensors as the Register and the original Tensors as the data

-- | ==== __Examples__
-- >>> getTensorFromTensors [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])]
-- ([(1,17),(0,18)],TensorMem [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(1,14),(0,17)],ByteMem [[16,17],[12,14]])])
getTensorFromTensors :: [Tensor] -> SortAlg -> Tensor
getTensorFromTensors tensors subAlg = (fromSortRec (subAlg (SortRec (getRegisterFromTensors tensors))), TensorMem tensors)

-- | For each Tensor, produces a Record by combining the top bit of the
--  Tensor with an index value for its Address

-- | Note that this output is not sorted. Sorting is done in the
--   getTensorFromTensors function

-- | ==== __Examples__
-- >>> getRegisterFromTensors [([(0,13),(1,18)],ByteMem [[11,13],[15,18]]),([(0,14),(1,17)],ByteMem [[12,14],[16,17]]),([(0,3),(1,7)],ByteMem [[1,3],[5,7]]),([(0,4),(1,8)],ByteMem [[2,4],[6,8]])]
-- [(0,18),(1,17),(2,7),(3,8)]
getRegisterFromTensors :: [Tensor] -> [Record]
getRegisterFromTensors tensors = acc tensors []
  where
    acc :: [Tensor] -> [Record] -> [Record]
    acc [] records = records
    acc (([], _) : remainingTensors) records = acc remainingTensors records
    acc (tensor : remainingTensors) records = acc remainingTensors (records ++ [(i, getTopBitFromTensorStack tensor)])
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
getTopBitFromTensorStack :: Tensor -> Int
getTopBitFromTensorStack (register, _) = snd (last register)
