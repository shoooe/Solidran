module Main where

import Solidran.Output (output)
import Solidran.Dna.Detail (nucleoCount)

main :: IO ()
main = getLine >>= (putStr . output . nucleoCount)
