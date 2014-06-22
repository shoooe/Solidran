module Main where

import Solidran.Fibd.Detail

main :: IO ()
main = do
    ln <- getLine
    let [n, m] = map read $ words ln
    print $ countRabbits n m
