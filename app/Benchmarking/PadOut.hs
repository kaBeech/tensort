module Benchmarking.PadOut (padOut) where

padOut :: String -> Int -> String
padOut s totalLength = s ++ replicate (totalLength - length s) ' '
