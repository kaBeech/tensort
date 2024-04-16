module Vanillasort (bsort, msort, qsort) where

import Bubblesort qualified (bsort)
import Mergesort qualified (msort)
import Quicksort qualified (qsort)

bsort :: (Ord array) => [array] -> [array]
bsort = Bubblesort.bsort

msort :: (Ord array) => [array] -> [array]
msort = Mergesort.msort

qsort :: (Ord array) => [array] -> [array]
qsort = Quicksort.qsort
