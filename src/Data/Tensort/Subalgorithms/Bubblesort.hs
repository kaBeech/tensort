module Data.Tensort.Subalgorithms.Bubblesort (bubblesort) where

import Data.Tensort.Utils.ComparisonFunctions (lessThanBit, lessThanRecord)
import Data.Tensort.Utils.Types (Sortable (..), WonkyState (..))

bubblesort :: Sortable -> WonkyState -> (Sortable, WonkyState)
bubblesort (SortBit bits) wonkySt = do
  let (bits', wonkySt') = bubblesortRepeater bits wonkySt lessThanBit (length bits)
  (SortBit bits', wonkySt')
bubblesort (SortRec recs) wonkySt = do
  let (recs', wonkySt') = bubblesortRepeater recs wonkySt lessThanRecord (length recs)
  (SortRec recs', wonkySt')

bubblesortRepeater :: [a] -> WonkyState -> (a -> a -> WonkyState -> (Bool, WonkyState)) -> Int -> ([a], WonkyState)
bubblesortRepeater recs wonkySt _ 0 = (recs, wonkySt)
bubblesortRepeater recs wonkySt lessThan i = do
  let (recs', wonkySt') = bubblesortSinglePass [] (head recs) (tail recs) lessThan wonkySt
  bubblesortRepeater recs' wonkySt' lessThan (i - 1)

bubblesortSinglePass :: [a] -> a -> [a] -> (a -> a -> WonkyState -> (Bool, WonkyState)) -> WonkyState -> ([a], WonkyState)
bubblesortSinglePass sortedSegment x [] _ wonkySt = (sortedSegment ++ [x], wonkySt)
bubblesortSinglePass sortedSegment x (y : unsortedSegment) lessThan wonkySt = do
  let (result, wonkySt') = lessThan x y wonkySt
  if result
    then bubblesortSinglePass (sortedSegment ++ [x]) y unsortedSegment lessThan wonkySt'
    else bubblesortSinglePass (sortedSegment ++ [y]) x unsortedSegment lessThan wonkySt'
