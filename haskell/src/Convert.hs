module Convert (intListToBoolList) where

intListToBoolList :: [Int] -> [Bool]
intListToBoolList = map even
