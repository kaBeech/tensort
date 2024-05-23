module Data.Robustsort.Utils.ComparisonFunctions (lessThanInt, lessThanRecord) where

import Data.Robustsort.Utils.Types (Record)

lessThanInt :: Int -> Int -> Bool
lessThanInt x y = x < y

lessThanRecord :: Record -> Record -> Bool
lessThanRecord x y = snd x < snd y
