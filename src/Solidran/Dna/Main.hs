module Main where

import qualified Solidran.Dna.Counter as Counter

main :: IO ()
main = getLine >>= (print . Counter.count)
