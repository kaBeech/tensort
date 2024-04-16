module Vanillasort (bsort, msort, qsort) where

import Bubblesort qualified (bsort)
import Mergesort qualified (msort)
import Quicksort qualified (qsort)

bsort :: IO ()
bsort = Bubblesort.bsort

msort :: IO ()
msort = Mergesort.msort

qsort :: (Ord array) => [array] -> [array]
qsort = Quicksort.qsort
