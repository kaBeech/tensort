module Data.Tensort.Utils.SimplifyRegister
  ( simplifyRegister,
    applySortingFromSimplifiedRegister,
  )
where

import qualified Data.Bifunctor
import Data.Tensort.Utils.Types (Record, RecordR)

simplifyRegister :: [RecordR] -> [Record]
simplifyRegister = map (Data.Bifunctor.second snd)

applySortingFromSimplifiedRegister :: [Record] -> [RecordR] -> [RecordR]
applySortingFromSimplifiedRegister
  sortedSimplifiedRegister
  unsortedRegiserR = do
    let registerR = acc sortedSimplifiedRegister [] unsortedRegiserR
    registerR
    where
      acc :: [Record] -> [RecordR] -> [RecordR] -> [RecordR]
      acc [] sortedRegisterR _ = sortedRegisterR
      acc (record : remainingRecords) sortedRegisterR unsortedRegiserR' = do
        let i = fst record
        let recordR = head (filter (\(i', _) -> i' == i) unsortedRegiserR')
        acc remainingRecords (sortedRegisterR ++ [recordR]) unsortedRegiserR'
