{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Test.SortSpec
  ( result_is_sorted_bits,
    result_is_sorted_bits_short,
    result_is_sorted_bits_tiny,
    result_is_sorted_records,
    result_is_sorted_records_short,
    result_is_sorted_records_tiny,
    result_is_sorted_custom_bitsize,
    result_is_sorted_bits_only,
    result_is_sorted_bits_only_short,
  )
where

import Data.Tensort.Tensort (tensortBN)
import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types (Bit, Record, SortAlg, Sortable (..))
import Test.QuickCheck

result_is_sorted_bits :: SortAlg -> [Bit] -> Property
result_is_sorted_bits sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted . sort $ SortBit unsortedList

result_is_sorted_bits_short :: SortAlg -> [Bit] -> Property
result_is_sorted_bits_short sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 6
    success = isSorted . sort $ SortBit unsortedList

result_is_sorted_bits_tiny :: SortAlg -> [Bit] -> Property
result_is_sorted_bits_tiny sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 3
    success = isSorted . sort $ SortBit unsortedList

result_is_sorted_records :: SortAlg -> [Record] -> Property
result_is_sorted_records sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted . sort $ SortRec unsortedList

result_is_sorted_records_short :: SortAlg -> [Record] -> Property
result_is_sorted_records_short sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 6
    success = isSorted . sort $ SortRec unsortedList

result_is_sorted_records_tiny :: SortAlg -> [Record] -> Property
result_is_sorted_records_tiny sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 3
    success = isSorted . sort $ SortRec unsortedList

result_is_sorted_custom_bitsize :: Int -> [Bit] -> Property
result_is_sorted_custom_bitsize n unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = (length unsortedList < 15) && (n > 1)
    success = isSorted (tensortBN n (SortBit unsortedList))

result_is_sorted_bits_only :: ([Bit] -> [Bit]) -> [Bit] -> Property
result_is_sorted_bits_only sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted . SortBit $ sort unsortedList

result_is_sorted_bits_only_short :: ([Bit] -> [Bit]) -> [Bit] -> Property
result_is_sorted_bits_only_short sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 6
    success = isSorted . SortBit $ sort unsortedList

limitStd :: Int
limitStd = 1000000
