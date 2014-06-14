module Solidran.Rna.Detail where

dnaToRna :: String -> String
dnaToRna = map toU
    where
        toU 'T' = 'U'
        toU  c  =  c
