module Main where

import Solidran.Output (output)
import Solidran.Dna.Nucleo (nucleoCount)

main :: IO ()
main = getLine >>= (putStr . output . nucleoCount)
