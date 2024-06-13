module SortSpec (result_is_sorted_bits, result_is_sorted_records, result_is_sorted_records_short) where

import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types (Bit, Record, SortAlg, Sortable (..))
import Test.QuickCheck

result_is_sorted_bits :: ([Bit] -> [Bit]) -> [Bit] -> Property
result_is_sorted_bits sort unsortedList = (length unsortedList < 10) && not (null unsortedList) ==> isSorted (SortBit (sort unsortedList))

result_is_sorted_records :: SortAlg -> [Record] -> Property
result_is_sorted_records sort unsortedList = (length unsortedList < 10) && not (null unsortedList) ==> isSorted (sort (SortRec unsortedList))

result_is_sorted_records_short :: SortAlg -> [Record] -> Property
result_is_sorted_records_short sort unsortedList = within 1000000 ((length unsortedList < 6) && not (null unsortedList) ==> isSorted (sort (SortRec unsortedList)))

result_is_sorted_sortable :: SortAlg -> Sortable -> Property
result_is_sorted_sortable sort (SortBit unsortedList) = (length unsortedList < 10) && not (null unsortedList) ==> isSorted (sort (SortBit unsortedList))
result_is_sorted_sortable sort (SortRec unsortedList) = (length unsortedList < 10) && not (null unsortedList) ==> isSorted (sort (SortRec unsortedList))
