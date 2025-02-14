module Data.Tensort.Utils.SortRecs (sortRecs) where

import Data.List (delete)
import Data.Tensort.Utils.Types (Bit, Record (..), SortAlg, fromRecord)

sortRecs :: (Ord a) => SortAlg a -> [Record a] -> [Record a]
sortRecs sortAlg recs =
  let topBits = map (fst . fromRecord) recs
      sortedTopBits = sortAlg topBits
      sortedRecs = orderRecsByTopBits sortedTopBits recs []
   in sortedRecs

orderRecsByTopBits :: (Ord a) => [Bit a] -> [Record a] -> [Record a] -> [Record a]
orderRecsByTopBits [] [] recs' = recs'
orderRecsByTopBits [_] [rec] recs' = recs' ++ [rec]
orderRecsByTopBits (topBit : topBits) recs recs' =
  let rec = head $ filter (\(x, _) -> x == topBit) (map fromRecord recs)
      recs'' = delete rec (map fromRecord recs)
   in orderRecsByTopBits topBits (map Record recs'') (recs' ++ [Record rec])
orderRecsByTopBits _ _ _ = error "orderRecsByTopBits: topBits and recs must be of equal length"
