module Test.QCheck
  ( qcheckSortable,
    qcheckSortableShort,
    qcheckSortableTiny,
    qcheckBits,
  )
where

import Data.Tensort.Utils.Types (Bit, SortAlg)
import Test.SortSpec
  ( result_is_sorted_bits,
    result_is_sorted_bits_only,
    result_is_sorted_records,
    result_is_sorted_records_short,
    result_is_sorted_records_tiny,
  )
import Test.TestCheck (check)

qcheckSortable :: (SortAlg, String) -> IO ()
qcheckSortable (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_bits sort)
  check (result_is_sorted_records sort)
  putStrLn "True!"

qcheckSortableShort :: (SortAlg, String) -> IO ()
qcheckSortableShort (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_records_short sort)
  putStrLn "True!"

qcheckSortableTiny :: (SortAlg, String) -> IO ()
qcheckSortableTiny (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_records_tiny sort)
  putStrLn "True!"

qcheckBits :: ([Bit] -> [Bit], String) -> IO ()
qcheckBits (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array..")
  check (result_is_sorted_bits_only sort)
  putStrLn "True!"
