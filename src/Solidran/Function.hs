module Solidran.Function 
    ( composeN
    ) where

composeN :: Int -> (a -> a) -> (a -> a)
composeN n fn = foldr (.) id (replicate n fn)
