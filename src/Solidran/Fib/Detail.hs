module Solidran.Fib.Detail where

import Solidran.Function (composeN)

nextPair :: Int -> (Int, Int) -> (Int, Int)
nextPair k (a, b) = (b, c)
    where c = b + (k * a)

rabbitsCount :: Int -> Int -> Int
rabbitsCount n k
    | n < 3     = 1
    | otherwise = snd . composeN (n - 2) (nextPair k) $ (1, 1)
