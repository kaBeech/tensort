module Data.Tensort
  ( tensort,
  )
where

import Data.Tensort.Tensort (tensortBL)
import Data.Tensort.Utils.Types (Bit)
import Data.Tensort.Utils.WrapSortAlg (wrapSortAlg)

tensort :: [Bit] -> [Bit]
tensort = wrapSortAlg tensortBL
