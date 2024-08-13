module SortSpec
  ( result_is_sorted_bits,
    result_is_sorted_records,
    result_is_sorted_records_short,
    result_is_sorted_custom_bitsize,
  )
where

import Data.Tensort.Tensort (tensortBN)
import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types (Bit, Record, SortAlg, Sortable (..))
import Test.QuickCheck

result_is_sorted_bits :: SortAlg -> [Bit] -> Property
result_is_sorted_bits sort unsortedList =
  within
    1000000
    ( (length unsortedList < 10) ==>
        isSorted (sort (SortBit unsortedList))
    )

result_is_sorted_records :: SortAlg -> [Record] -> Property
result_is_sorted_records sort unsortedList =
  within
    1000000
    ( (length unsortedList < 10) ==>
        isSorted (sort (SortRec unsortedList))
    )

result_is_sorted_records_short :: SortAlg -> [Record] -> Property
result_is_sorted_records_short sort unsortedList =
  within
    1000000
    ( (length unsortedList < 6) ==>
        isSorted (sort (SortRec unsortedList))
    )

result_is_sorted_custom_bitsize :: Int -> [Bit] -> Property
result_is_sorted_custom_bitsize n unsortedList =
  within
    1000000
    ( (length unsortedList < 15) && (n > 1) ==>
        isSorted (tensortBN n (SortBit unsortedList))
    )
