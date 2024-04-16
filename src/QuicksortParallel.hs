module QuicksortParallel (qsortp) where

qsortp :: (Ord array) => [array] -> [array]
qsortp [] = []
qsortp (element : elements) =
  let lowerPartition1 = qsortp [array | array <- elements, array <= element]
      upperPartition1 = qsortp [array | array <- elements, array > element]
      lowerPartition2 = qsortp [array | array <- elements, array <= element]
      upperPartition2 = qsortp [array | array <- elements, array > element]
   in if length upperPartition1 == length upperPartition2 && length lowerPartition1 == length lowerPartition2
        then lowerPartition1 ++ [element] ++ upperPartition1
        else qsortp (element : elements)
