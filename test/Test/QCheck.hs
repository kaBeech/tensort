module Test.QCheck
  ( qcheckBitsInt,
    qcheckRecsShortInt,
    qcheckRecsTinyInt,
    qcheckBitsOnlyInt,
    qcheckRecsInt,
  )
where

import Data.Tensort.Utils.Types (SortAlg)
import Test.SortSpec
  ( result_is_sorted_bits_only_ints,
    result_is_sorted_ints,
    result_is_sorted_records_ints,
    result_is_sorted_records_ints_short,
    result_is_sorted_records_ints_tiny,
  )
import Test.TestCheck (check)

qcheckBitsInt :: (SortAlg Int, String) -> IO ()
qcheckBitsInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_ints sort)
  putStrLn "True!"

qcheckRecsInt :: (SortAlg (Int, Int), String) -> IO ()
qcheckRecsInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_records_ints sort)
  putStrLn "True!"

qcheckRecsShortInt :: (SortAlg (Int, Int), String) -> IO ()
qcheckRecsShortInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_records_ints_short sort)
  putStrLn "True!"

qcheckRecsTinyInt :: (SortAlg (Int, Int), String) -> IO ()
qcheckRecsTinyInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_records_ints_tiny sort)
  putStrLn "True!"

qcheckBitsOnlyInt :: ([Int] -> [Int], String) -> IO ()
qcheckBitsOnlyInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_bits_only_ints sort)
  putStrLn "True!"
