module Solidran.Revc.Detail where

complementDna :: String -> String
complementDna = reverse . map comp
    where
        comp 'A' = 'T'
        comp 'T' = 'A'
        comp 'C' = 'G'
        comp 'G' = 'C'
        comp  c  =  c
