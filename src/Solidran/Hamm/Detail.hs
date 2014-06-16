module Solidran.Hamm.Detail where

hammingDist :: String -> String -> Int
hammingDist a b = foldr fn 0 z
    where 
        fn (x, y) c
            | x == y    = c
            | otherwise = c + 1
        z = zip a b
