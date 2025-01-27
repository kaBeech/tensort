-- | This module provides functions to simplify sorting Registers in Tensort
--   variants that are sorting Records in their input instead of Bits.
module Data.Tensort.Utils.SimplifyRegister
  ( simplifyRegister,
    applySortingFromSimplifiedRegister,
  )
where

import qualified Data.Bifunctor
import Data.Tensort.Utils.Types (Record, RecordR)

-- | For use in Tensort variants that are sorting Records in their input
--   instead of Bits.
--
--   This function simplifies the Register used for sorting Records by
--   replacing the TopRecord of each RecordR with its TopBit. This returns
--   a Register of standard Records that can be used for sorting.
simplifyRegister :: [RecordR] -> [Record]
simplifyRegister = map (Data.Bifunctor.second snd)

-- | For use in Tensort variants that are sorting Records in their input
--  instead of Bits.
--
--  This function takes a simplified Register that has been sorted and the
--  unsimplified, unsorted, original RegisterR. It puts the elements of the
--  original RegisterR in the same order as the simplified, sorted Register
--  and returns the sorted RegisterR.
applySortingFromSimplifiedRegister :: [Record] -> [RecordR] -> [RecordR]
applySortingFromSimplifiedRegister sortedSimplifiedRegister = acc sortedSimplifiedRegister []
  where
    acc :: [Record] -> [RecordR] -> [RecordR] -> [RecordR]
    acc [] sortedRegisterR _ = sortedRegisterR
    acc (record : remainingRecords) sortedRegisterR unsortedRegiserR' = do
      let i = fst record
      let recordR = head (filter (\(i', _) -> i' == i) unsortedRegiserR')
      acc remainingRecords (sortedRegisterR ++ [recordR]) unsortedRegiserR'
