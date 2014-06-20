module Main where

import Solidran.Prot.Detail

main :: IO ()
main = getLine >>= (putStrLn . encode)
