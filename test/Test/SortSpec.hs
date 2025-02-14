{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Test.SortSpec
  ( result_is_sorted_ints,
    result_is_sorted_ints_short,
    result_is_sorted_ints_tiny,
    result_is_sorted_records_ints,
    result_is_sorted_records_ints_short,
    result_is_sorted_records_ints_tiny,
    result_is_sorted_custom_bitsize_ints,
    result_is_sorted_bits_only_ints,
    result_is_sorted_bits_only_ints_short,
    result_is_sorted_chars,
    result_is_sorted_strings,
    result_is_sorted_lists,
    result_is_sorted_generic,
  )
where

import Data.Tensort.Tensort (tensortBN)
import Data.Tensort.Utils.Check (isSorted)
import Data.Tensort.Utils.Types (SortAlg)
import Test.QuickCheck

result_is_sorted_ints :: SortAlg Int -> [Int] -> Property
result_is_sorted_ints sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted $ sort unsortedList

result_is_sorted_ints_short :: SortAlg Int -> [Int] -> Property
result_is_sorted_ints_short sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 6
    success = isSorted $ sort unsortedList

result_is_sorted_ints_tiny :: SortAlg Int -> [Int] -> Property
result_is_sorted_ints_tiny sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 3
    success = isSorted $ sort unsortedList

result_is_sorted_records_ints :: SortAlg (Int, Int) -> [(Int, Int)] -> Property
result_is_sorted_records_ints sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted $ sort unsortedList

result_is_sorted_records_ints_short :: SortAlg (Int, Int) -> [(Int, Int)] -> Property
result_is_sorted_records_ints_short sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 6
    success = isSorted $ sort unsortedList

result_is_sorted_records_ints_tiny :: SortAlg (Int, Int) -> [(Int, Int)] -> Property
result_is_sorted_records_ints_tiny sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 3
    success = isSorted $ sort unsortedList

result_is_sorted_custom_bitsize_ints :: Int -> [Int] -> Property
result_is_sorted_custom_bitsize_ints n unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = (length unsortedList < 15) && (n > 1)
    success = isSorted (tensortBN n unsortedList)

result_is_sorted_bits_only_ints :: ([Int] -> [Int]) -> [Int] -> Property
result_is_sorted_bits_only_ints sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted $ sort unsortedList

result_is_sorted_bits_only_ints_short :: ([Int] -> [Int]) -> [Int] -> Property
result_is_sorted_bits_only_ints_short sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 6
    success = isSorted $ sort unsortedList

result_is_sorted_chars :: SortAlg Char -> [Char] -> Property
result_is_sorted_chars sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted $ sort unsortedList

result_is_sorted_strings :: SortAlg String -> [String] -> Property
result_is_sorted_strings sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted $ sort unsortedList

result_is_sorted_lists :: (Ord a) => SortAlg [a] -> [[a]] -> Property
result_is_sorted_lists sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted $ sort unsortedList

result_is_sorted_generic :: (Ord a) => SortAlg a -> [a] -> Property
result_is_sorted_generic sort unsortedList =
  within limitStd (constraint ==> success)
  where
    constraint = length unsortedList < 10
    success = isSorted $ sort unsortedList

limitStd :: Int
limitStd = 1000000
