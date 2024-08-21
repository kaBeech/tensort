module Data.Tensort.Utils.WrapSortAlg
  ( wrapSortAlg,
  )
where

import Data.Tensort.Utils.Types (Bit, SortAlg, Sortable (SortBit), fromSortBit)

wrapSortAlg :: SortAlg -> ([Bit] -> [Bit])
wrapSortAlg sortAlg xs = fromSortBit (sortAlg (SortBit xs))
