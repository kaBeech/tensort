module Test.QCheck
  ( qcheckBitsInt,
    qcheckRecsShortInt,
    qcheckRecsTinyInt,
    qcheckBitsOnlyInt,
    qcheckRecsInt,
    qcheckBitsChar,
    qcheckBitsString,
    qcheckBitsList,
    qcheckBitsFloat,
    qcheckBitsDouble,
    qcheckBitsInteger,
    qcheckBitsRational,
    qcheckBitsBool,
    qcheckBitsWord,
    qcheckBitsOrdering,
    qcheckBitsMaybe,
    qcheckBitsEither,
    qcheckBitsTuple,
  )
where

import Data.Tensort.Utils.Types (SortAlg)
import Test.SortSpec
  ( result_is_sorted_bits_only_ints,
    result_is_sorted_generic,
    result_is_sorted_records_ints,
    result_is_sorted_records_ints_short,
    result_is_sorted_records_ints_tiny,
  )
import Test.TestCheck (check)

qcheckBitsInt :: (SortAlg Int, String) -> IO ()
qcheckBitsInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Ints..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckRecsInt :: (SortAlg (Int, Int), String) -> IO ()
qcheckRecsInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Ints..")
  check (result_is_sorted_records_ints sort)
  putStrLn "True!"

qcheckRecsShortInt :: (SortAlg (Int, Int), String) -> IO ()
qcheckRecsShortInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Ints..")
  check (result_is_sorted_records_ints_short sort)
  putStrLn "True!"

qcheckRecsTinyInt :: (SortAlg (Int, Int), String) -> IO ()
qcheckRecsTinyInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Ints..")
  check (result_is_sorted_records_ints_tiny sort)
  putStrLn "True!"

qcheckBitsOnlyInt :: ([Int] -> [Int], String) -> IO ()
qcheckBitsOnlyInt (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Ints..")
  check (result_is_sorted_bits_only_ints sort)
  putStrLn "True!"

qcheckBitsInteger :: (SortAlg Integer, String) -> IO ()
qcheckBitsInteger (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Integers..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsFloat :: (SortAlg Float, String) -> IO ()
qcheckBitsFloat (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Floats..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsDouble :: (SortAlg Double, String) -> IO ()
qcheckBitsDouble (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Doubles..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsRational :: (SortAlg Rational, String) -> IO ()
qcheckBitsRational (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Rationals..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsChar :: (SortAlg Char, String) -> IO ()
qcheckBitsChar (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Chars..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsString :: (SortAlg String, String) -> IO ()
qcheckBitsString (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Strings..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsBool :: (SortAlg Bool, String) -> IO ()
qcheckBitsBool (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Bools..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsWord :: (SortAlg Word, String) -> IO ()
qcheckBitsWord (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Words..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsOrdering :: (SortAlg Ordering, String) -> IO ()
qcheckBitsOrdering (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Orderings..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsMaybe :: (SortAlg (Maybe Int), String) -> IO ()
qcheckBitsMaybe (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Maybes..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsEither :: (SortAlg (Either Int Char), String) -> IO ()
qcheckBitsEither (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Eithers..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsTuple :: (SortAlg (Int, Int), String) -> IO ()
qcheckBitsTuple (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Tuples..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"

qcheckBitsList :: (SortAlg [Int], String) -> IO ()
qcheckBitsList (sort, sortName) = do
  putStrLn (sortName ++ " returns a sorted array with Lists..")
  check (result_is_sorted_generic sort)
  putStrLn "True!"
