-- | This module provides convenience functions to wrap sorting algorithms
--   that use the Sortable type so they can be used without worrying about
--   type conversion
module Data.Tensort.Utils.WrapSortAlg
  ( wrapSortAlg,
  )
where

import Data.Tensort.Utils.Types (Bit, SortAlg, Sortable (SortBit), WonkyState, fromSortBit)

-- | Wraps a sorting algorithm that uses the Sortable type so it can be used
--   to sort Bits without worrying about type conversion

-- | ==== __Examples__
--  >>> import Data.Tensort.Robustsort (robustsortM)
--  >>> (wrapSortAlg robustsortM) [16, 23, 4, 8, 15, 42]
--  [4,8,15,16,23,42]
wrapSortAlg :: WonkyState -> SortAlg -> ([Bit] -> [Bit])
wrapSortAlg wonkySt sortAlg xs =
  let (result, _) = sortAlg wonkySt (SortBit xs)
   in fromSortBit result
