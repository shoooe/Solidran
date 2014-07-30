module Main where

import Solidran.Mrna.Detail

main :: IO ()
main = getLine >>= (print . rnaCombsMod 1000000)
