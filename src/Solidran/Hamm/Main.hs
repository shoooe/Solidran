module Main where

import Solidran.Hamm.Detail

main :: IO ()
main = do
    c <- getContents
    let [a, b] = lines c
    putStr . show $ hammingDist a b
