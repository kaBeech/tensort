module Data.Tensort
  ( tensort,
  )
where

import Data.Tensort.Tensort (tensortBL)
import Data.Tensort.Utils.Types (Bit, Sortable (..), fromSortBit)

tensort :: [Bit] -> [Bit]
tensort xs = fromSortBit (tensortBL (SortBit xs))
