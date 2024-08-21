module Data.Robustsort
  ( robustsortP,
    robustsortB,
    robustsortM,
    robustsortRP,
    robustsortRB,
    robustsortRM,
  )
where

import qualified Data.Tensort.Robustsort
  ( robustsortB,
    robustsortM,
    robustsortP,
    robustsortRB,
    robustsortRM,
    robustsortRP,
  )
import Data.Tensort.Utils.Types (Bit)
import Data.Tensort.Utils.WrapSortAlg (wrapSortAlg)

robustsortP :: [Bit] -> [Bit]
robustsortP = wrapSortAlg Data.Tensort.Robustsort.robustsortP

robustsortB :: [Bit] -> [Bit]
robustsortB = wrapSortAlg Data.Tensort.Robustsort.robustsortB

robustsortM :: [Bit] -> [Bit]
robustsortM = wrapSortAlg Data.Tensort.Robustsort.robustsortM

robustsortRP :: [Bit] -> [Bit]
robustsortRP = wrapSortAlg Data.Tensort.Robustsort.robustsortRP

robustsortRB :: [Bit] -> [Bit]
robustsortRB = wrapSortAlg Data.Tensort.Robustsort.robustsortRB

robustsortRM :: [Bit] -> [Bit]
robustsortRM = wrapSortAlg Data.Tensort.Robustsort.robustsortRM
